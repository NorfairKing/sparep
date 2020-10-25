{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Sparep.Data.StudyUnitId where

import Control.Monad
import Crypto.Hash.SHA256 as SHA256
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.ByteString.Base16
import Data.List.NonEmpty as NE
import Data.Proxy
import Data.Semigroup
import Data.Text
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Validity
import Data.Validity.ByteString ()
import Data.Validity.Path ()
import Data.Validity.Text ()
import Database.Persist
import Database.Persist.Sql
import GHC.Generics (Generic)
import Sparep.Data.Card
import Sparep.Data.FillExercise
import Sparep.Data.StudyUnit

hashStudyUnit :: StudyUnit -> StudyUnitId
hashStudyUnit su =
  let bs = case su of
        CardUnit c -> cardContents c
        FillExerciseUnit fe -> fillExerciseContents fe
   in StudyUnitId {cardIdSha256 = SHA256.hash bs}

cardContents :: Card -> ByteString
cardContents Card {..} =
  let sideBytes = \case
        TextSide t -> TE.encodeUtf8 (T.strip t)
        SoundSide _ contents -> contents
   in SB.concat
        [ sideBytes cardFront,
          sideBytes cardBack
        ]

fillExerciseContents :: FillExercise -> ByteString
fillExerciseContents FillExercise {..} = sconcat $ NE.map fillExercisePartContents fillExerciseSequence

fillExercisePartContents :: FillExercisePart -> ByteString
fillExercisePartContents = \case
  LitPart t -> SB.cons 0 $ TE.encodeUtf8 t
  FillPart t -> SB.cons 1 $ TE.encodeUtf8 t

newtype StudyUnitId
  = StudyUnitId
      { cardIdSha256 :: ByteString
      }
  deriving (Show, Eq, Ord, Generic)

instance Validity StudyUnitId where
  validate cid@StudyUnitId {..} =
    mconcat
      [ genericValidate cid,
        declare "The length of the sha256 hash bytestring is 32 bytes" $
          SB.length cardIdSha256 == 32
      ]

renderStudyUnitIdHex :: StudyUnitId -> Text
renderStudyUnitIdHex = encodeBase16 . cardIdSha256

parseStudyUnitIdHex :: Text -> Either Text StudyUnitId
parseStudyUnitIdHex = fmap StudyUnitId . decodeBase16 . TE.encodeUtf8

instance FromJSON StudyUnitId where
  parseJSON = withText "StudyUnitId" $ \t -> case parseStudyUnitIdHex t of
    Left err -> fail $ T.unpack err
    Right cid -> pure cid

instance ToJSON StudyUnitId where
  toJSON = toJSON . renderStudyUnitIdHex

renderStudyUnitId :: StudyUnitId -> ByteString
renderStudyUnitId StudyUnitId {..} = cardIdSha256

parseStudyUnitId :: ByteString -> Either Text StudyUnitId
parseStudyUnitId sb =
  case SB.length sb of
    32 ->
      pure StudyUnitId {cardIdSha256 = sb}
    l
      | l > 32 ->
        pure StudyUnitId {cardIdSha256 = SB.take 32 sb}
      | otherwise -> Left $ "Invalid card id length: " <> T.pack (show l)

instance PersistField StudyUnitId where
  toPersistValue = toPersistValue . renderStudyUnitId

  fromPersistValue = fromPersistValue >=> parseStudyUnitId

instance PersistFieldSql StudyUnitId where
  sqlType Proxy = sqlType (Proxy :: Proxy ByteString)

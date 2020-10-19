{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Sparep.Data.CardId where

import Control.Monad
import Crypto.Hash.SHA256 as SHA256
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.ByteString.Base16
import Data.Proxy
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

hashCard :: Card -> CardId
hashCard Card {..} =
  let sideBytes = \case
        TextSide t -> TE.encodeUtf8 (T.strip t)
        SoundSide _ contents -> contents
      bs =
        SB.concat
          [ sideBytes cardFront,
            sideBytes cardBack
          ]
   in CardId {cardIdSha256 = SHA256.hash bs}

newtype CardId
  = CardId
      { cardIdSha256 :: ByteString
      }
  deriving (Show, Eq, Ord, Generic)

instance Validity CardId where
  validate cid@CardId {..} =
    mconcat
      [ genericValidate cid,
        declare "The length of the sha256 hash bytestring is 32 bytes" $
          SB.length cardIdSha256 == 32
      ]

renderCardIdHex :: CardId -> Text
renderCardIdHex = encodeBase16 . cardIdSha256

parseCardIdHex :: Text -> Either Text CardId
parseCardIdHex = fmap CardId . decodeBase16 . TE.encodeUtf8

instance FromJSON CardId where
  parseJSON = withText "CardId" $ \t -> case parseCardIdHex t of
    Left err -> fail $ T.unpack err
    Right cid -> pure cid

instance ToJSON CardId where
  toJSON = toJSON . renderCardIdHex

renderCardId :: CardId -> ByteString
renderCardId CardId {..} = cardIdSha256

parseCardId :: ByteString -> Either Text CardId
parseCardId sb =
  case SB.length sb of
    32 ->
      pure CardId {cardIdSha256 = sb}
    l
      | l > 32 ->
        pure CardId {cardIdSha256 = SB.take 32 sb}
      | otherwise -> Left $ "Invalid card id length: " <> T.pack (show l)

instance PersistField CardId where
  toPersistValue = toPersistValue . renderCardId

  fromPersistValue = fromPersistValue >=> parseCardId

instance PersistFieldSql CardId where
  sqlType Proxy = sqlType (Proxy :: Proxy ByteString)

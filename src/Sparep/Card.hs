{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Sparep.Card where

import Control.Monad
import Crypto.Hash.SHA256 as SHA256
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Builder as SBB
import qualified Data.ByteString.Lazy as LB
import Data.Maybe
import Data.Proxy
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Validity
import Data.Validity.ByteString ()
import Data.Validity.Text ()
import Data.Word
import Data.Yaml
import Database.Persist
import Database.Persist.Sql
import GHC.Generics (Generic)
import YamlParse.Applicative

data CardDefs
  = CardDefs
      { cardDefsCards :: [CardDef],
        cardDefsReverse :: Bool
      }
  deriving (Show, Eq, Generic)

instance Validity CardDefs

instance YamlSchema CardDefs where
  yamlSchema =
    objectParser "CardDefs" $
      CardDefs
        <$> optionalFieldWithDefault "cards" [] "Card definitions"
        <*> optionalFieldWithDefault "reverse" False "Whether to generate reverse cards"

instance FromJSON CardDefs where
  parseJSON = viaYamlSchema

data CardDef
  = CardDef
      { cardDefFront :: Text,
        cardDefBack :: Text,
        cardDefReverse :: Maybe Bool
      }
  deriving (Show, Eq, Generic)

instance Validity CardDef

instance YamlSchema CardDef where
  yamlSchema =
    objectParser "CardDef" $
      CardDef
        <$> requiredField "front" "The front of the card"
        <*> requiredField "back" "The back of the card"
        <*> optionalField "reverse" "Whether to also generate the reverse card"

instance FromJSON CardDef where
  parseJSON = viaYamlSchema

resolveCardDefs :: CardDefs -> [Card]
resolveCardDefs CardDefs {..} = concatMap (resolveCardDef cardDefsReverse) cardDefsCards

resolveCardDef :: Bool -> CardDef -> [Card]
resolveCardDef defaultReverse CardDef {..} =
  let rightWayRoundCard = Card {cardFront = cardDefFront, cardBack = cardDefBack}
      reversedCard = reverseCard rightWayRoundCard
      doReversal = fromMaybe defaultReverse cardDefReverse
   in [rightWayRoundCard] ++ [reversedCard | doReversal]

reverseCard :: Card -> Card
reverseCard c =
  Card
    { cardFront = cardBack c,
      cardBack = cardFront c
    }

data Card
  = Card
      { cardFront :: Text,
        cardBack :: Text
      }
  deriving (Show, Eq, Generic)

instance Validity Card

hashCard :: Card -> CardId
hashCard Card {..} =
  let bs =
        SB.concat
          [ TE.encodeUtf8 cardFront,
            TE.encodeUtf8 cardBack
          ]
   in CardId
        { cardIdSha256 = SHA256.hash bs,
          cardIdLength = fromIntegral $ SB.length bs
        }

data CardId
  = CardId
      { cardIdSha256 :: !ByteString,
        cardIdLength :: !Word16
      }
  deriving (Show, Eq, Generic)

instance Validity CardId where
  validate cid@CardId {..} =
    mconcat
      [ genericValidate cid,
        declare "The length of the sha256 hash bytestring is 32 bytes" $ SB.length cardIdSha256 == 32
      ]

renderCardId :: CardId -> ByteString
renderCardId CardId {..} =
  LB.toStrict $ SBB.toLazyByteString $
    mconcat
      [ SBB.byteString cardIdSha256,
        SBB.word16BE cardIdLength
      ]

parseCardId :: ByteString -> Either Text CardId
parseCardId sb = case SB.length sb of
  34 ->
    let ws = SB.unpack sb
        (shaBytes, lengthBytes) = splitAt 32 ws
        cardIdSha256 = SB.pack shaBytes
     in case lengthBytes of
          [lb1, lb2] -> do
            let lb116 = fromIntegral lb1 :: Word16
            let lb216 = fromIntegral lb2 :: Word16
            let cardIdLength = shiftL lb116 8 + lb216
            pure CardId {..}
          _ -> Left $ "Invalid number of length bytes, not 2"
  l -> Left $ "Invalid card id length: " <> T.pack (show l)

instance PersistField CardId where
  toPersistValue = toPersistValue . renderCardId
  fromPersistValue = fromPersistValue >=> parseCardId

instance PersistFieldSql CardId where
  sqlType Proxy = sqlType (Proxy :: Proxy ByteString)

data Difficulty
  = CardIncorrect
  | CardCorrect
  | CardEasy
  deriving (Show, Eq, Generic)

instance Validity Difficulty

renderDifficulty :: Difficulty -> Text
renderDifficulty = \case
  CardIncorrect -> "Incorrect"
  CardCorrect -> "Correct"
  CardEasy -> "Easy"

parseDifficulty :: Text -> Either Text Difficulty
parseDifficulty = \case
  "Incorrect" -> Right CardIncorrect
  "Correct" -> Right CardCorrect
  "Easy" -> Right CardEasy
  _ -> Left "Unknown Difficulty"

instance PersistField Difficulty where
  toPersistValue = toPersistValue . renderDifficulty
  fromPersistValue = fromPersistValue >=> parseDifficulty

instance PersistFieldSql Difficulty where
  sqlType Proxy = sqlType (Proxy :: Proxy Text)

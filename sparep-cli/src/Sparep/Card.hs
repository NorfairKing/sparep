{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Sparep.Card where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Crypto.Hash.SHA256 as SHA256
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Builder as SBB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import Data.Proxy
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Validity
import Data.Validity.ByteString ()
import Data.Validity.Containers ()
import Data.Validity.Path ()
import Data.Validity.Text ()
import Data.Word
import Data.Yaml
import Database.Persist
import Database.Persist.Sql
import GHC.Generics (Generic)
import Path
import Path.IO
import YamlParse.Applicative

data Deck
  = Deck
      { deckName :: !(Maybe Text),
        deckDescription :: !(Maybe Text),
        deckInstructions :: !(Maybe Instructions),
        deckReverse :: !(Maybe Bool),
        deckCards :: ![CardDef]
      }
  deriving (Show, Eq, Generic)

instance Validity Deck

instance YamlSchema Deck where
  yamlSchema =
    objectParser "Deck" $
      Deck
        <$> optionalField "name" "Name of the deck"
        <*> optionalField "description" "Description of the deck"
        <*> optionalField "instructions" "Instructions for what to do when you see the front of the card"
        <*> optionalField "reverse" "Whether to generate reverse cards"
        <*> optionalFieldWithDefault "cards" [] "Card definitions"

instance FromJSON Deck where
  parseJSON = viaYamlSchema

data CardDef
  = CardFrontBack CardFrontBackDef
  | CardManySided CardManySidedDef
  deriving (Show, Eq, Generic)

instance Validity CardDef

instance YamlSchema CardDef where
  yamlSchema =
    alternatives
      [ CardFrontBack <$> yamlSchema,
        CardManySided <$> yamlSchema
      ]

instance FromJSON CardDef where
  parseJSON = viaYamlSchema

data CardFrontBackDef
  = CardFrontBackDef
      { cardFrontBackDefFront :: !CardSideDef,
        cardFrontBackDefBack :: !CardSideDef,
        cardFrontBackDefReverse :: !(Maybe Bool),
        cardFrontBackDefInstructions :: !(Maybe Instructions)
      }
  deriving (Show, Eq, Generic)

instance Validity CardFrontBackDef

instance YamlSchema CardFrontBackDef where
  yamlSchema =
    objectParser "CardFrontBackDef" $
      CardFrontBackDef
        <$> requiredField "front" "The front of the card"
        <*> requiredField "back" "The back of the card"
        <*> optionalField "reverse" "Whether to also generate the reverse card"
        <*> optionalField "instructions" "Instructions for what to do when you see the front of the card"

instance FromJSON CardFrontBackDef where
  parseJSON = viaYamlSchema

newtype Instructions = Instructions {unInstructions :: Text}
  deriving (Show, Eq, Generic)

instance Validity Instructions

instance YamlSchema Instructions where
  yamlSchema = Instructions <$> yamlSchema

instance FromJSON Instructions where
  parseJSON = viaYamlSchema

data CardManySidedDef
  = CardManySidedDef
      { cardManySidedDefSides :: !(Map Text CardSideDef),
        cardManySidedDefInstructions :: Maybe Instructions
      }
  deriving (Show, Eq, Generic)

instance Validity CardManySidedDef

instance YamlSchema CardManySidedDef where
  yamlSchema =
    objectParser "CardManySidedDef" $
      CardManySidedDef
        <$> requiredField "sides" "The sides of the many-sided card"
        <*> optionalField "instructions" "Instructions for what to do when you see the front of the card"

data CardSideDef
  = TextSideDef Text
  | SoundSideDef FilePath
  deriving (Show, Eq, Generic)

instance Validity CardSideDef

instance YamlSchema CardSideDef where
  yamlSchema =
    alternatives
      [ TextSideDef <$> yamlSchema,
        objectParser "SoundSideDef" $
          ( SoundSideDef
              <$ requiredFieldWith "type" "Declare that it's a sound" (literalString "sound")
          )
            <*> requiredField "path" "The path to the sound file, from the deck definition"
      ]

instance FromJSON CardSideDef where
  parseJSON = viaYamlSchema

resolveDeck :: MonadIO m => Deck -> m [Card]
resolveDeck Deck {..} =
  concat <$> mapM (resolveCardDef deckReverse deckInstructions) deckCards

resolveCardDef :: MonadIO m => Maybe Bool -> Maybe Instructions -> CardDef -> m [Card]
resolveCardDef mDefaultReverse mDefaultInstructions = \case
  CardFrontBack cfbd -> resolveCardFrontBackDef mDefaultReverse mDefaultInstructions cfbd
  CardManySided cmsd -> resolveCardManySidedDef mDefaultInstructions cmsd

resolveCardFrontBackDef :: MonadIO m => Maybe Bool -> Maybe Instructions -> CardFrontBackDef -> m [Card]
resolveCardFrontBackDef mDefaultReverse mDefaultInstructions CardFrontBackDef {..} = do
  let mInstructions = cardFrontBackDefInstructions <|> mDefaultInstructions
      defaultReverse = fromMaybe False mDefaultReverse
  frontSide <- resolveCardSideDef cardFrontBackDefFront
  backSide <- resolveCardSideDef cardFrontBackDefBack
  let rightWayRoundCard =
        Card
          { cardInstructions = unInstructions <$> mInstructions,
            cardFront = frontSide,
            cardBack = backSide
          }
      reversedCard =
        Card
          { cardInstructions = unInstructions <$> mInstructions,
            cardFront = backSide,
            cardBack = frontSide
          }
      doReversal = fromMaybe defaultReverse cardFrontBackDefReverse
  pure $ rightWayRoundCard : [reversedCard | doReversal]

resolveCardManySidedDef :: MonadIO m => Maybe Instructions -> CardManySidedDef -> m [Card]
resolveCardManySidedDef mDefaultInstructions CardManySidedDef {..} = do
  let mInstructions = cardManySidedDefInstructions <|> mDefaultInstructions
  sidesList <- M.toList <$> mapM resolveCardSideDef cardManySidedDefSides
  pure $ do
    s1@(side1Name, side1) <- sidesList
    (side2Name, side2) <- filter (/= s1) sidesList
    pure
      Card
        { cardInstructions =
            Just $ T.unwords $
              concat
                [ [unInstructions i | i <- maybeToList mInstructions],
                  ["(", side1Name, "->", side2Name, ")"]
                ],
          cardFront = side1,
          cardBack = side2
        }

resolveCardSideDef :: MonadIO m => CardSideDef -> m CardSide
resolveCardSideDef = \case
  TextSideDef t -> pure $ TextSide $ T.strip t
  SoundSideDef fp -> SoundSide <$> resolveFile' fp

data Card
  = Card
      { cardInstructions :: !(Maybe Text),
        cardFront :: !CardSide,
        cardBack :: !CardSide
      }
  deriving (Show, Eq, Generic)

instance Validity Card

hashCard :: Card -> CardId
hashCard Card {..} =
  let sideBytes = \case
        TextSide t -> TE.encodeUtf8 (T.strip t)
        SoundSide fp -> TE.encodeUtf8 $ T.pack $ "sound: " <> fromAbsFile fp
      bs =
        SB.concat
          [ sideBytes cardFront,
            sideBytes cardBack
          ]
   in CardId
        { cardIdSha256 = SHA256.hash bs,
          cardIdLength = fromIntegral $ SB.length bs
        }

data CardSide
  = TextSide Text
  | SoundSide (Path Abs File)
  deriving (Show, Eq, Generic)

instance Validity CardSide

instance YamlSchema CardSide where
  yamlSchema =
    alternatives
      [ TextSide <$> yamlSchema,
        objectParser "SoundSide" $
          ( SoundSide
              <$ requiredFieldWith "type" "Declare that it's a sound" (literalString "sound")
          )
            <*> requiredField "path" "The path to the sound file, from the deck definition"
      ]

instance FromJSON CardSide where
  parseJSON = viaYamlSchema

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
        declare "The length of the sha256 hash bytestring is 32 bytes" $
          SB.length cardIdSha256 == 32
      ]

renderCardId :: CardId -> ByteString
renderCardId CardId {..} =
  LB.toStrict
    $ SBB.toLazyByteString
    $ mconcat [SBB.byteString cardIdSha256, SBB.word16BE cardIdLength]

parseCardId :: ByteString -> Either Text CardId
parseCardId sb =
  case SB.length sb of
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
            _ -> Left "Invalid number of length bytes, not 2"
    l -> Left $ "Invalid card id length: " <> T.pack (show l)

instance PersistField CardId where
  toPersistValue = toPersistValue . renderCardId

  fromPersistValue = fromPersistValue >=> parseCardId

instance PersistFieldSql CardId where
  sqlType Proxy = sqlType (Proxy :: Proxy ByteString)

data Difficulty
  = CardIncorrect
  | CardHard
  | CardGood
  | CardEasy
  deriving (Show, Eq, Generic)

instance Validity Difficulty

renderDifficulty :: Difficulty -> Text
renderDifficulty =
  \case
    CardIncorrect -> "Incorrect"
    CardHard -> "Hard"
    CardGood -> "Good"
    CardEasy -> "Easy"

parseDifficulty :: Text -> Either Text Difficulty
parseDifficulty =
  \case
    "Incorrect" -> Right CardIncorrect
    "Hard" -> Right CardHard
    "Good" -> Right CardGood
    "Correct" -> Right CardGood
    "Easy" -> Right CardEasy
    _ -> Left "Unknown Difficulty"

instance PersistField Difficulty where
  toPersistValue = toPersistValue . renderDifficulty

  fromPersistValue = fromPersistValue >=> parseDifficulty

instance PersistFieldSql Difficulty where
  sqlType Proxy = sqlType (Proxy :: Proxy Text)

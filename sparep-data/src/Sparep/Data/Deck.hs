{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Sparep.Data.Deck where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as SB
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import Data.Validity
import Data.Validity.ByteString ()
import Data.Validity.Containers ()
import Data.Validity.Path ()
import Data.Validity.Text ()
import Data.Yaml as Yaml
import GHC.Generics (Generic)
import Path
import Path.IO
import Sparep.Data.Card
import System.Exit
import YamlParse.Applicative as YamlParse

data RootedDeck
  = RootedDeck
      { rootedDeckPath :: !(Path Abs File),
        rootedDeckDeck :: !Deck
      }
  deriving (Show, Eq, Generic)

instance Validity RootedDeck

readRootedDeck :: Path Abs File -> IO (Maybe RootedDeck)
readRootedDeck afp = do
  md <- YamlParse.readConfigFile afp
  pure $ (\d -> RootedDeck {rootedDeckPath = afp, rootedDeckDeck = d}) <$> md

readRootedDeckOrDie :: Path Abs File -> IO RootedDeck
readRootedDeckOrDie afp = do
  md <- readRootedDeck afp
  case md of
    Nothing -> die $ "Deck not found: " <> fromAbsFile afp
    Just rd -> pure rd

readRootedDeckOrErr :: Path Abs File -> IO (Maybe (Either Yaml.ParseException RootedDeck))
readRootedDeckOrErr afp = do
  mContents <- forgivingAbsence $ SB.readFile $ fromAbsFile afp
  forM mContents $ \contents -> pure $ case Yaml.decodeEither' contents of
    Left err -> Left err
    Right d -> Right $ RootedDeck {rootedDeckPath = afp, rootedDeckDeck = d}

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
  = CardFrontBack !CardFrontBackDef
  | CardManySided !CardManySidedDef
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
        cardManySidedDefInstructions :: !(Maybe Instructions)
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
  = TextSideDef !Text
  | SoundSideDef !FilePath
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

resolveRootedDeck :: MonadIO m => RootedDeck -> m [Card]
resolveRootedDeck RootedDeck {..} = resolveDeck rootedDeckPath rootedDeckDeck

resolveDeck :: MonadIO m => Path Abs File -> Deck -> m [Card]
resolveDeck fp Deck {..} =
  concat <$> mapM (resolveCardDef fp deckReverse deckInstructions) deckCards

resolveCardDef :: MonadIO m => Path Abs File -> Maybe Bool -> Maybe Instructions -> CardDef -> m [Card]
resolveCardDef fp mDefaultReverse mDefaultInstructions = \case
  CardFrontBack cfbd -> resolveCardFrontBackDef fp mDefaultReverse mDefaultInstructions cfbd
  CardManySided cmsd -> resolveCardManySidedDef fp mDefaultInstructions cmsd

resolveCardFrontBackDef :: MonadIO m => Path Abs File -> Maybe Bool -> Maybe Instructions -> CardFrontBackDef -> m [Card]
resolveCardFrontBackDef fp mDefaultReverse mDefaultInstructions CardFrontBackDef {..} = do
  let mInstructions = cardFrontBackDefInstructions <|> mDefaultInstructions
      defaultReverse = fromMaybe False mDefaultReverse
  frontSide <- resolveCardSideDef fp cardFrontBackDefFront
  backSide <- resolveCardSideDef fp cardFrontBackDefBack
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

resolveCardManySidedDef :: MonadIO m => Path Abs File -> Maybe Instructions -> CardManySidedDef -> m [Card]
resolveCardManySidedDef fp mDefaultInstructions CardManySidedDef {..} = do
  let mInstructions = cardManySidedDefInstructions <|> mDefaultInstructions
  sidesList <- M.toList <$> mapM (resolveCardSideDef fp) cardManySidedDefSides
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

resolveCardSideDef :: MonadIO m => Path Abs File -> CardSideDef -> m CardSide
resolveCardSideDef dfp = \case
  TextSideDef t -> pure $ TextSide $ T.strip t
  SoundSideDef fp -> liftIO $ do
    afp <- resolveFile (parent dfp) fp
    contents <- SB.readFile $ fromAbsFile afp
    pure $ SoundSide afp contents

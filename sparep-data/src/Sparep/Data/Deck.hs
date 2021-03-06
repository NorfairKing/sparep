{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Sparep.Data.Deck where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as SB
import Data.List.NonEmpty ((<|), NonEmpty (..))
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
import Sparep.Data.DeckName
import Sparep.Data.DefinitionContext
import Sparep.Data.FillExercise
import Sparep.Data.Instructions
import Sparep.Data.StudyUnit
import System.Directory as FP
import System.Exit
import YamlParse.Applicative as YamlParse

parseDecks :: FilePath -> IO [RootedDeck]
parseDecks fp' = do
  fp <- normaliseDeckFilePath fp'
  fileExists <- FP.doesFileExist fp
  if fileExists
    then do
      p <- resolveFile' fp
      maybeToList <$> readRootedDeck p
    else do
      dirExists <- FP.doesDirectoryExist fp
      if dirExists
        then do
          p <- resolveDir' fp
          fs <- snd <$> listDirRecur p
          fmap catMaybes
            $ forM fs
            $ \f -> do
              errOrDefs <- readRootedDeckOrErr f
              pure $ either (const Nothing) Just =<< errOrDefs
        else pure []

normaliseDeckFilePath :: FilePath -> IO FilePath
normaliseDeckFilePath fp = case fp of
  '~' : '/' : rest -> do
    home <- getHomeDir
    let fp' = fromAbsDir home ++ rest
    pure fp'
  _ -> pure fp

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

resolveRootedDeck :: MonadIO m => RootedDeck -> m [DefinitionContext StudyUnit]
resolveRootedDeck RootedDeck {..} = resolveDeck rootedDeckPath rootedDeckDeck

data Deck
  = Deck
      { deckName :: !(Maybe DeckName),
        deckDescription :: !(Maybe Text),
        deckInstructions :: !(Maybe Instructions),
        deckReverse :: !(Maybe Bool),
        deckStudyUnits :: ![StudyUnitDef]
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

resolveDeck :: MonadIO m => Path Abs File -> Deck -> m [DefinitionContext StudyUnit]
resolveDeck fp Deck {..} = concat <$> mapM (resolveStudyUnitDef fp deckName deckReverse deckInstructions) deckStudyUnits

data StudyUnitDef
  = CardUnitDef !CardDef
  | FillExerciseUnitDef !FillExerciseDef
  deriving (Show, Eq, Generic)

instance Validity StudyUnitDef

instance YamlSchema StudyUnitDef where
  yamlSchema =
    alternatives
      [ FillExerciseUnitDef <$> yamlSchema,
        CardUnitDef <$> yamlSchema
      ]

resolveStudyUnitDef :: MonadIO m => Path Abs File -> Maybe DeckName -> Maybe Bool -> Maybe Instructions -> StudyUnitDef -> m [DefinitionContext StudyUnit]
resolveStudyUnitDef fp mDeckName mDefaultReverse mDefaultInstructions = \case
  CardUnitDef cd -> map (fmap CardUnit) <$> resolveCardDef fp mDeckName mDefaultReverse mDefaultInstructions cd
  FillExerciseUnitDef fed -> pure [FillExerciseUnit <$> resolveFillExerciseDef mDeckName mDefaultInstructions fed]

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

resolveCardDef :: MonadIO m => Path Abs File -> Maybe DeckName -> Maybe Bool -> Maybe Instructions -> CardDef -> m [DefinitionContext Card]
resolveCardDef fp mDeckName mDefaultReverse mDefaultInstructions = \case
  CardFrontBack cfbd -> resolveCardFrontBackDef fp mDeckName mDefaultReverse mDefaultInstructions cfbd
  CardManySided cmsd -> resolveCardManySidedDef fp mDeckName mDefaultInstructions cmsd

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

resolveCardFrontBackDef :: MonadIO m => Path Abs File -> Maybe DeckName -> Maybe Bool -> Maybe Instructions -> CardFrontBackDef -> m [DefinitionContext Card]
resolveCardFrontBackDef fp mDeckName mDefaultReverse mDefaultInstructions CardFrontBackDef {..} = do
  let mInstructions = cardFrontBackDefInstructions <|> mDefaultInstructions
      defaultReverse = fromMaybe False mDefaultReverse
  frontSide <- resolveCardSideDef fp cardFrontBackDefFront
  backSide <- resolveCardSideDef fp cardFrontBackDefBack
  let withCtx c =
        DefinitionContext
          { definitionContextUnit = c,
            definitionContextDeckName = mDeckName,
            definitionContextInstructions = mInstructions
          }
      rightWayRoundCard =
        Card
          { cardFront = frontSide,
            cardBack = backSide
          }
      reversedCard =
        Card
          { cardFront = backSide,
            cardBack = frontSide
          }
      doReversal = fromMaybe defaultReverse cardFrontBackDefReverse
  pure $ map withCtx $ rightWayRoundCard : [reversedCard | doReversal]

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

resolveCardManySidedDef :: MonadIO m => Path Abs File -> Maybe DeckName -> Maybe Instructions -> CardManySidedDef -> m [DefinitionContext Card]
resolveCardManySidedDef fp mDeckName mDefaultInstructions CardManySidedDef {..} = do
  let mInstructions = cardManySidedDefInstructions <|> mDefaultInstructions
  sidesList <- M.toList <$> mapM (resolveCardSideDef fp) cardManySidedDefSides
  pure $ do
    s1@(side1Name, side1) <- sidesList
    (side2Name, side2) <- filter (/= s1) sidesList
    pure $
      DefinitionContext
        { definitionContextUnit =
            Card
              { cardFront = side1,
                cardBack = side2
              },
          definitionContextDeckName = mDeckName,
          definitionContextInstructions =
            Just $ Instructions $ T.unwords $
              concat
                [ [i | Instructions i <- maybeToList mInstructions],
                  ["(", side1Name, "->", side2Name, ")"]
                ]
        }

data CardSideDef
  = TextSideDef !Text
  | SoundSideDef !FilePath
  | ImageSideDef !FilePath
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
            <*> requiredField "path" "The path to the sound file, from the deck definition",
        objectParser "ImageSideDef" $
          ( ImageSideDef
              <$ requiredFieldWith "type" "Declare that it's an image" (literalString "image")
          )
            <*> requiredField "path" "The path to the image file, from the deck definition"
      ]

instance FromJSON CardSideDef where
  parseJSON = viaYamlSchema

resolveCardSideDef :: MonadIO m => Path Abs File -> CardSideDef -> m CardSide
resolveCardSideDef dfp = \case
  TextSideDef t -> pure $ TextSide $ T.strip t
  SoundSideDef fp -> liftIO $ do
    afp <- resolveFile (parent dfp) fp
    contents <- SB.readFile $ fromAbsFile afp
    pure $ SoundSide afp contents
  ImageSideDef fp -> liftIO $ do
    afp <- resolveFile (parent dfp) fp
    contents <- SB.readFile $ fromAbsFile afp
    pure $ ImageSide afp contents

data FillExerciseDef
  = FillExerciseDef
      { fillExerciseDefSequence :: NonEmpty FillExercisePart,
        fillExerciseDefInstructions :: Maybe Instructions
      }
  deriving (Show, Eq, Generic)

instance Validity FillExerciseDef

instance YamlSchema FillExerciseDef where
  yamlSchema =
    alternatives
      [ FillExerciseDef <$> (parseFillExerciseParts <$> yamlSchema) <*> pure Nothing,
        objectParser "FillExerciseDef" $
          FillExerciseDef
            <$> requiredFieldWith "fill" "The text to fill in" (parseFillExerciseParts <$> yamlSchema)
            <*> optionalField "instructions" "Instructions for this specific fill exercise"
      ]

resolveFillExerciseDef :: Maybe DeckName -> Maybe Instructions -> FillExerciseDef -> DefinitionContext FillExercise
resolveFillExerciseDef mDeckName mDefaultInstructions FillExerciseDef {..} =
  DefinitionContext
    { definitionContextUnit =
        FillExercise
          { fillExerciseSequence = fillExerciseDefSequence
          },
      definitionContextDeckName = mDeckName,
      definitionContextInstructions = fillExerciseDefInstructions <|> mDefaultInstructions
    }

parseFillExerciseParts :: Text -> NonEmpty FillExercisePart
parseFillExerciseParts = goLit [] . T.unpack
  where
    goLit acc [] = LitPart (T.pack $ reverse acc) :| []
    goLit acc ('_' : rest) = LitPart (T.pack $ reverse acc) <| goFill [] rest
    goLit acc (c : rest) = goLit (c : acc) rest
    goFill acc [] = LitPart (T.pack $ reverse acc) :| []
    goFill acc ('_' : rest) = FillPart (T.pack $ reverse acc) <| goLit [] rest
    goFill acc (c : rest) = goFill (c : acc) rest

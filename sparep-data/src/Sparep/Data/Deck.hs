{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Sparep.Data.Deck where

import Autodocodec
import Autodocodec.Yaml
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as SB
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.ByteString ()
import Data.Validity.Containers ()
import Data.Validity.Path ()
import Data.Validity.Text ()
import Data.Yaml as Yaml (FromJSON, ParseException, ToJSON, decodeEither')
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
          fmap catMaybes $
            forM fs $
              \f -> do
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

data RootedDeck = RootedDeck
  { rootedDeckPath :: !(Path Abs File),
    rootedDeckDeck :: !Deck
  }
  deriving (Show, Eq, Generic)

instance Validity RootedDeck

readRootedDeck :: Path Abs File -> IO (Maybe RootedDeck)
readRootedDeck afp = do
  md <- readYamlConfigFile afp
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

data Deck = Deck
  { deckName :: !(Maybe DeckName),
    deckDescription :: !(Maybe Text),
    deckInstructions :: !(Maybe Instructions),
    deckReverse :: !(Maybe Bool),
    deckStudyUnits :: ![StudyUnitDef]
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Deck)

instance Validity Deck

instance HasCodec Deck where
  codec =
    object "Deck" $
      Deck
        <$> optionalField "name" "Name of the deck" .= deckName
        <*> optionalField "description" "Description of the deck" .= deckDescription
        <*> optionalField "instructions" "Instructions for what to do when you see the front of the card" .= deckInstructions
        <*> optionalField "reverse" "Whether to generate reverse cards" .= deckReverse
        <*> optionalFieldWithDefault "cards" [] "Card definitions" .= deckStudyUnits

resolveDeck :: MonadIO m => Path Abs File -> Deck -> m [DefinitionContext StudyUnit]
resolveDeck fp Deck {..} = concat <$> mapM (resolveStudyUnitDef fp deckName deckReverse deckInstructions) deckStudyUnits

data StudyUnitDef
  = CardUnitDef !CardDef
  | FillExerciseUnitDef !FillExerciseDef
  deriving (Show, Eq, Generic)

instance Validity StudyUnitDef

instance HasCodec StudyUnitDef where
  codec = dimapCodec f g $ eitherCodec codec codec
    where
      f = \case
        Left cd -> CardUnitDef cd
        Right fed -> FillExerciseUnitDef fed
      g = \case
        CardUnitDef cd -> Left cd
        FillExerciseUnitDef fed -> Right fed

resolveStudyUnitDef :: MonadIO m => Path Abs File -> Maybe DeckName -> Maybe Bool -> Maybe Instructions -> StudyUnitDef -> m [DefinitionContext StudyUnit]
resolveStudyUnitDef fp mDeckName mDefaultReverse mDefaultInstructions = \case
  CardUnitDef cd -> map (fmap CardUnit) <$> resolveCardDef fp mDeckName mDefaultReverse mDefaultInstructions cd
  FillExerciseUnitDef fed -> pure [FillExerciseUnit <$> resolveFillExerciseDef mDeckName mDefaultInstructions fed]

data CardDef
  = CardFrontBack !CardFrontBackDef
  | CardManySided !CardManySidedDef
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec CardDef)

instance Validity CardDef

instance HasCodec CardDef where
  codec = dimapCodec f g $ eitherCodec codec codec
    where
      f = \case
        Left cfbd -> CardFrontBack cfbd
        Right cmsd -> CardManySided cmsd
      g = \case
        CardFrontBack cfbd -> Left cfbd
        CardManySided cmsd -> Right cmsd

resolveCardDef :: MonadIO m => Path Abs File -> Maybe DeckName -> Maybe Bool -> Maybe Instructions -> CardDef -> m [DefinitionContext Card]
resolveCardDef fp mDeckName mDefaultReverse mDefaultInstructions = \case
  CardFrontBack cfbd -> resolveCardFrontBackDef fp mDeckName mDefaultReverse mDefaultInstructions cfbd
  CardManySided cmsd -> resolveCardManySidedDef fp mDeckName mDefaultInstructions cmsd

data CardFrontBackDef = CardFrontBackDef
  { cardFrontBackDefFront :: !CardSideDef,
    cardFrontBackDefBack :: !CardSideDef,
    cardFrontBackDefReverse :: !(Maybe Bool),
    cardFrontBackDefInstructions :: !(Maybe Instructions)
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec CardFrontBackDef)

instance Validity CardFrontBackDef

instance HasCodec CardFrontBackDef where
  codec =
    object "CardFrontBackDef" $
      CardFrontBackDef
        <$> requiredField "front" "The front of the card" .= cardFrontBackDefFront
        <*> requiredField "back" "The back of the card" .= cardFrontBackDefBack
        <*> optionalField "reverse" "Whether to also generate the reverse card" .= cardFrontBackDefReverse
        <*> optionalField "instructions" "Instructions for what to do when you see the front of the card" .= cardFrontBackDefInstructions

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

data CardManySidedDef = CardManySidedDef
  { cardManySidedDefSides :: !(Map Text CardSideDef),
    cardManySidedDefInstructions :: !(Maybe Instructions)
  }
  deriving (Show, Eq, Generic)

instance Validity CardManySidedDef

instance HasCodec CardManySidedDef where
  codec =
    object "CardManySidedDef" $
      CardManySidedDef
        <$> requiredField "sides" "The sides of the many-sided card" .= cardManySidedDefSides
        <*> optionalField "instructions" "Instructions for what to do when you see the front of the card" .= cardManySidedDefInstructions

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
            Just $
              Instructions $
                T.unwords $
                  concat
                    [ [i | Instructions i <- maybeToList mInstructions],
                      ["(", side1Name, "->", side2Name, ")"]
                    ]
        }

data CardSideDef
  = TextSideDef !Text
  | SoundSideDef !FilePath
  | ImageSideDef !FilePath
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec CardSideDef)

instance Validity CardSideDef

instance HasCodec CardSideDef where
  codec = dimapCodec f g $ eitherCodec codec $ eitherCodec soundSideDefCodec imageSideDefCodec
    where
      f = \case
        Left t -> TextSideDef t
        Right (Left ssd) -> ssd
        Right (Right isd) -> isd
      g = \case
        TextSideDef t -> Left t
        SoundSideDef ssd -> Right (Left ssd)
        ImageSideDef isd -> Right (Right isd)
      soundSideDefCodec =
        object "SoundSideDef" $
          typeField "sound" SoundSideDef
            <*> requiredField "path" "The path to the sound file, from the deck definition"
      imageSideDefCodec =
        object "ImageSideDef" $
          typeField "image" ImageSideDef
            <*> requiredField "path" "The path to the image file, from the deck definition"

      typeField :: Text -> a -> ObjectCodec b a
      typeField typeName a =
        a <$ requiredFieldWith' "type" (literalTextCodec typeName) .= const typeName

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

data FillExerciseDef = FillExerciseDef
  { fillExerciseDefSequence :: NonEmpty FillExercisePart,
    fillExerciseDefInstructions :: Maybe Instructions
  }
  deriving (Show, Eq, Generic)

instance Validity FillExerciseDef

instance HasCodec FillExerciseDef where
  codec =
    dimapCodec f g $
      eitherCodec fillExercisePartsCodec $
        object "FillExerciseDef" $
          FillExerciseDef
            <$> requiredFieldWith "fill" fillExercisePartsCodec "The text to fill in" .= fillExerciseDefSequence
            <*> optionalField "instructions" "Instructions for this specific fill exercise" .= fillExerciseDefInstructions
    where
      f = \case
        Left parts -> FillExerciseDef parts Nothing
        Right fed -> fed
      g fed = case fillExerciseDefInstructions fed of
        Nothing -> Left (fillExerciseDefSequence fed)
        _ -> Right fed

fillExercisePartsCodec :: JSONCodec (NonEmpty FillExercisePart)
fillExercisePartsCodec = dimapCodec parseFillExerciseParts renderFillExerciseParts codec

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

renderFillExerciseParts :: NonEmpty FillExercisePart -> Text
renderFillExerciseParts = T.concat . map go . NE.toList
  where
    go :: FillExercisePart -> Text
    go = \case
      LitPart t -> t
      FillPart t -> "_" <> t <> "_"

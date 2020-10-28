{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Sparep.TUI.State where

import Cursor.List.NonEmpty
import qualified Cursor.Simple.List.NonEmpty as Simple
import Cursor.Text
import Data.Maybe
import Data.Text (Text)
import Data.Time
import Sparep.Client.Data
import Sparep.Data

data Loading a = Loading | Loaded a
  deriving (Show, Eq)

data State
  = StateMenu MenuState
  | StateDecks DecksState
  | StateStudyUnits StudyUnitsState
  | StateStudy StudyState
  deriving (Show, Eq)

data MenuState
  = MenuState
      { menuStateDecks :: [RootedDeck],
        menuStateSelection :: Loading (Selection (DefinitionContext StudyUnit))
      }
  deriving (Show, Eq)

data DecksState
  = DecksState
      { decksStateCursor :: !(Maybe (Simple.NonEmptyCursor (RootedDeck, Loading (Selection (DefinitionContext StudyUnit)))))
      }
  deriving (Show, Eq)

data StudyUnitsState
  = StudyUnitsState
      { studyUnitsStateDeck :: !RootedDeck,
        studyUnitsStateCursor :: !(Maybe (Simple.NonEmptyCursor (DefinitionContext StudyUnit, Loading (Maybe (UTCTime, UTCTime)))))
      }
  deriving (Show, Eq)

-- TODO see if this ungodly mess can be refactored.
data StudyState
  = StudyState
      { studyStateCursor ::
          !( Loading
               ( Maybe
                   ( NonEmptyCursor
                       ( StudyContext
                           ( DefinitionContext StudyUnitCursor
                           )
                       )
                       ( StudyContext
                           ( DefinitionContext StudyUnit
                           )
                       )
                   )
               )
           ),
        studyStateRepetitions :: ![ClientRepetition]
      }
  deriving (Show, Eq)

data StudyUnitCursor
  = CardUnitCursor !CardCursor
  | FillExerciseUnitCursor !FillExerciseCursor
  deriving (Show, Eq)

makeStudyUnitCursor :: StudyUnit -> StudyUnitCursor
makeStudyUnitCursor = \case
  CardUnit c -> CardUnitCursor $ makeCardCursor c
  FillExerciseUnit fe -> FillExerciseUnitCursor $ makeFillExerciseCursor fe

rebuildStudyUnitCursor :: StudyUnitCursor -> StudyUnit
rebuildStudyUnitCursor = \case
  CardUnitCursor cc -> CardUnit $ rebuildCardCursor cc
  FillExerciseUnitCursor fec -> FillExerciseUnit $ rebuildFillExerciseCursor fec

data CardCursor
  = CardCursor
      { cardCursorCard :: !Card,
        cardCursorFrontBack :: !FrontBack
      }
  deriving (Show, Eq)

makeCardCursor :: Card -> CardCursor
makeCardCursor c = CardCursor {cardCursorCard = c, cardCursorFrontBack = Front}

cardCursorShowBack :: CardCursor -> CardCursor
cardCursorShowBack cc = cc {cardCursorFrontBack = Back}

rebuildCardCursor :: CardCursor -> Card
rebuildCardCursor = cardCursorCard

data FillExerciseCursor
  = FillExerciseCursor
      { fillExerciseCursorList :: Simple.NonEmptyCursor FillExercisePartCursor,
        fillExerciseCursorShow :: Bool -- True means shown
      }
  deriving (Show, Eq)

makeFillExerciseCursor :: FillExercise -> FillExerciseCursor
makeFillExerciseCursor FillExercise {..} =
  let fec =
        FillExerciseCursor
          { fillExerciseCursorList = Simple.makeNonEmptyCursor $ fmap makeFillExercisePartCursor fillExerciseSequence,
            fillExerciseCursorShow = False
          }
   in fromMaybe fec $ fillExerciseCursorSeek fec

rebuildFillExerciseCursor :: FillExerciseCursor -> FillExercise
rebuildFillExerciseCursor FillExerciseCursor {..} =
  FillExercise
    { fillExerciseSequence = rebuildFillExercisePartCursor <$> Simple.rebuildNonEmptyCursor fillExerciseCursorList
    }

fillExerciseCursorSeek :: FillExerciseCursor -> Maybe FillExerciseCursor
fillExerciseCursorSeek = fillExerciseCursorSeekHelper Simple.nonEmptyCursorSelectNext

fillExerciseCursorSeekBack :: FillExerciseCursor -> Maybe FillExerciseCursor
fillExerciseCursorSeekBack = fillExerciseCursorSeekHelper Simple.nonEmptyCursorSelectPrev

fillExerciseCursorSeekHelper :: (forall a. Simple.NonEmptyCursor a -> Maybe (Simple.NonEmptyCursor a)) -> FillExerciseCursor -> Maybe FillExerciseCursor
fillExerciseCursorSeekHelper func fec = do
  nec <- func (fillExerciseCursorList fec)
  (\c -> fec {fillExerciseCursorList = c}) <$> go nec
  where
    go :: Simple.NonEmptyCursor FillExercisePartCursor -> Maybe (Simple.NonEmptyCursor FillExercisePartCursor)
    go nec =
      if fillExercisePartCursorIsFill (nonEmptyCursorCurrent nec)
        then pure nec
        else func nec >>= go

fillExerciseCursorCountHoles :: FillExerciseCursor -> Word
fillExerciseCursorCountHoles = foldNonEmptyCursor (\befores cur afters -> sum $ map count $ befores ++ [cur] ++ afters) . fillExerciseCursorList
  where
    count :: FillExercisePartCursor -> Word
    count = \case
      LitPartCursor _ -> 0
      FillPartCursor _ _ -> 1

fillExerciseCursorCorrect :: FillExerciseCursor -> Bool
fillExerciseCursorCorrect = foldNonEmptyCursor (\befores cur afters -> all fillExercisePartCursorCorrect $ befores ++ [cur] ++ afters) . fillExerciseCursorList

data FillExercisePartCursor
  = LitPartCursor !Text
  | FillPartCursor !TextCursor !Text
  deriving (Show, Eq)

makeFillExercisePartCursor :: FillExercisePart -> FillExercisePartCursor
makeFillExercisePartCursor = \case
  LitPart t -> LitPartCursor t
  FillPart t -> FillPartCursor emptyTextCursor t

rebuildFillExercisePartCursor :: FillExercisePartCursor -> FillExercisePart
rebuildFillExercisePartCursor = \case
  LitPartCursor t -> LitPart t
  FillPartCursor _ t -> FillPart t

fillExercisePartCursorIsFill :: FillExercisePartCursor -> Bool
fillExercisePartCursorIsFill = \case
  FillPartCursor _ _ -> True
  _ -> False

fillExercisePartCursorCorrect :: FillExercisePartCursor -> Bool
fillExercisePartCursorCorrect = \case
  LitPartCursor _ -> True
  FillPartCursor tc t -> t == rebuildTextCursor tc

data FrontBack
  = Front
  | Back
  deriving (Show, Eq)

data ResourceName
  = TextCursorName
  deriving (Show, Eq, Ord)

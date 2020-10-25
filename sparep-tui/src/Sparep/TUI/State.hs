{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Sparep.TUI.State where

import Cursor.List.NonEmpty
import qualified Cursor.Simple.List.NonEmpty as Simple
import Cursor.Text
import Data.Text (Text)
import Data.Time
import Sparep.Client.Data
import Sparep.Data
import Sparep.TUI.Repetition

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
        menuStateSelection :: Loading (Selection StudyUnit)
      }
  deriving (Show, Eq)

data DecksState
  = DecksState
      { decksStateCursor :: !(Maybe (Simple.NonEmptyCursor (RootedDeck, Loading (Selection StudyUnit))))
      }
  deriving (Show, Eq)

data StudyUnitsState
  = StudyUnitsState
      { studyUnitsStateDeck :: !RootedDeck,
        studyUnitsStateCursor :: !(Maybe (Simple.NonEmptyCursor (StudyUnit, Loading (Maybe (UTCTime, UTCTime)))))
      }
  deriving (Show, Eq)

data StudyState
  = StudyState
      { studyStateCursor :: !(Loading (Maybe (NonEmptyCursor StudyUnitCursor StudyUnit))),
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

newtype FillExerciseCursor
  = FillExerciseCursor
      { fillExerciseCursorList :: NonEmptyCursor FillExercisePartCursor FillExercisePart
      }
  deriving (Show, Eq)

makeFillExerciseCursor :: FillExercise -> FillExerciseCursor
makeFillExerciseCursor FillExercise {..} = FillExerciseCursor $ makeNonEmptyCursor makeFillExercisePartCursor fillExerciseSequence

rebuildFillExerciseCursor :: FillExerciseCursor -> FillExercise
rebuildFillExerciseCursor = FillExercise . rebuildNonEmptyCursor rebuildFillExercisePartCursor . fillExerciseCursorList

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

data FrontBack
  = Front
  | Back
  deriving (Show, Eq)

data ResourceName
  = TextCursorName
  deriving (Show, Eq, Ord)

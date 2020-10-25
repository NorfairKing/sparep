{-# LANGUAGE LambdaCase #-}

module Sparep.TUI.State where

import Cursor.List.NonEmpty
import qualified Cursor.Simple.List.NonEmpty as Simple
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
  deriving (Show, Eq)

makeStudyUnitCursor :: StudyUnit -> StudyUnitCursor
makeStudyUnitCursor = \case
  CardUnit c -> CardUnitCursor $ makeCardCursor c

rebuildStudyUnitCursor :: StudyUnitCursor -> StudyUnit
rebuildStudyUnitCursor = \case
  CardUnitCursor cc -> CardUnit $ rebuildCardCursor cc

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

data FrontBack
  = Front
  | Back
  deriving (Show, Eq)

data ResourceName
  = ResourceName
  deriving (Show, Eq, Ord)

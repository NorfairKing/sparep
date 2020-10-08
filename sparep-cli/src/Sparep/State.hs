module Sparep.State where

import Cursor.Simple.List.NonEmpty
import Data.Time
import Sparep.Card
import Sparep.DB
import Sparep.Repetition

data Loading a = Loading | Loaded a
  deriving (Show, Eq)

data State
  = StateMenu MenuState
  | StateDecks DecksState
  | StateCards CardsState
  | StateStudy StudyState
  deriving (Show, Eq)

data MenuState
  = MenuState
      { menuStateDecks :: [Deck]
      }
  deriving (Show, Eq)

data DecksState
  = DecksState
      { decksStateCursor :: !(Maybe (NonEmptyCursor (Deck, Loading (Selection Card))))
      }
  deriving (Show, Eq)

data CardsState
  = CardsState
      { cardsStateDeck :: !Deck,
        cardsStateCursor :: !(Maybe (NonEmptyCursor (Card, Loading (Maybe (UTCTime, UTCTime)))))
      }
  deriving (Show, Eq)

data StudyState
  = StudyState
      { studyStateCursor :: !(Loading (Maybe (NonEmptyCursor Card))),
        studyStateFrontBack :: !FrontBack,
        studyStateRepetitions :: ![Repetition]
      }
  deriving (Show, Eq)

data FrontBack
  = Front
  | Back
  deriving (Show, Eq)

data ResourceName
  = ResourceName
  deriving (Show, Eq, Ord)

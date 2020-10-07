module Sparep.State where

import Cursor.Simple.List.NonEmpty
import Data.Time
import Sparep.Card
import Sparep.Repetition

data State
  = StateMenu MenuState
  | StateDecks DecksState
  | StateCards CardsState
  | StateStudy StudyState

data MenuState
  = MenuState
      { menuStateDecks :: [Deck]
      }
  deriving (Show, Eq)

data DecksState
  = DecksState
      { decksStateCursor :: NonEmptyCursor (Deck, Selection Card)
      }
  deriving (Show, Eq)

data CardsState
  = CardsState
      { cardsStateDeck :: Deck,
        cardsStateCursor :: Maybe (NonEmptyCursor (Card, Maybe UTCTime, Maybe UTCTime))
      }
  deriving (Show, Eq)

data StudyState
  = StudyState
      { studyStateCursor :: Maybe (NonEmptyCursor Card),
        studyStateFrontBack :: FrontBack
      }
  deriving (Show, Eq)

data FrontBack
  = Front
  | Back
  deriving (Show, Eq)

data ResourceName
  = ResourceName
  deriving (Show, Eq, Ord)

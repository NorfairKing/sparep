module Sparep where

import Prelude
import Effect.Aff (Aff)
import Halogen as H
import Control.Monad.State (modify_)
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Data.Symbol (SProxy(..))

_header :: SProxy "header"
_header = SProxy

type State
  = { cards :: Array Card
    }

data Action
  = Init

component :: forall q i o. H.Component HH.HTML q i o Aff
component =
  H.mkComponent
    { initialState:
      \_ ->
        { cards: []
        }
    , render: render
    , eval:
      H.mkEval
        $ H.defaultEval
            { handleAction = handle
            , initialize = Just Init
            }
    }

render :: forall q. State -> H.ComponentHTML Action q Aff
render s =
  HH.div_
    [ HH.h1_ [ HH.text "Spaced out" ]
    , HH.h2_ [ HH.text "Cards" ]
    , HH.div_ (map renderCard s.cards)
    ]

renderCard :: forall a q. Card -> H.ComponentHTML a q Aff
renderCard c =
  HH.div_
    [ HH.text "front: "
    , HH.text c.cardFront
    , HH.text " back: "
    , HH.text c.cardBack
    ]

handle :: forall q o. Action -> H.HalogenM State Action q o Aff Unit
handle = case _ of
  Init -> modify_ (_ { cards = exampleCards })

type Card
  = { cardFront :: String, cardBack :: String }

exampleCards :: Array Card
exampleCards =
  [ { cardFront: "hello", cardBack: "world" }
  , { cardFront: "foo", cardBack: "bar" }
  ]

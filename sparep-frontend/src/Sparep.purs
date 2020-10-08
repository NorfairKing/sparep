module Sparep where

import Prelude
import Effect.Aff (Aff)
import Halogen as H
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Data.Symbol (SProxy(..))

_header :: SProxy "header"
_header = SProxy

type State
  = { 
    }

data Action
  = Init

component :: forall q i o. H.Component HH.HTML q i o Aff
component =
  H.mkComponent
    { initialState:
      \_ ->
        {}
    , render: render
    , eval:
      H.mkEval
        $ H.defaultEval
            { handleAction = handle
            , initialize = Just Init
            }
    }

render :: forall q. State -> H.ComponentHTML Action q Aff
render _ = HH.text "Hello world"

handle :: forall q o. Action -> H.HalogenM State Action q o Aff Unit
handle = case _ of
  Init -> pure unit

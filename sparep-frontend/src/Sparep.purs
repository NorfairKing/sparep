module Sparep where

import Prelude
import Effect.Aff (Aff)
import Halogen as H
import Control.Monad.State (modify_)
import Data.Array (cons, head, reverse, intercalate)
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Data.Symbol (SProxy(..))

_header :: SProxy "header"
_header = SProxy

type State
  = { path :: Array Deck
    }

data Action
  = Init | LaunchDeck Deck

component :: forall q i o. H.Component HH.HTML q i o Aff
component =
  H.mkComponent
    { initialState:
      \_ ->
        { path: [homeDeck]
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
render s = case head s.path of
    Nothing -> renderError
    Just d -> renderDeck s d

renderNavBar :: forall q. String -> H.ComponentHTML Action q Aff
renderNavBar t =
  HH.div [ HP.class_ (HC.ClassName "navBar"), HE.onClick \_ -> Just Init ]
    [ HH.img [ HP.src "/sprites/logo.svg", HP.class_ (HC.ClassName "logo")]
    , HH.h1_ [ HH.text t ]
    ]

renderDeckPreviewTile :: forall q. Deck -> H.ComponentHTML Action q Aff
renderDeckPreviewTile d@(MakeDeck {name, decks, cards}) =
  HH.div_
    [
        HH.div [ HP.class_ (HC.ClassName "deck"), HE.onClick \_ -> Just $ LaunchDeck d]
          [
            HH.p_ [ HH.text name ]
          ]
    ]

renderError:: forall q. H.ComponentHTML Action q Aff
renderError =
  HH.div_
    [ renderNavBar "Spaced Out"
    ]

renderDeck:: forall q. State -> Deck -> H.ComponentHTML Action q Aff
renderDeck s d@(MakeDeck {name, decks, cards}) =
  HH.div_
    [ renderNavBar "Spaced Out"
    , renderDeckPreviewBar d
    , HH.div [ HP.class_ (HC.ClassName "content") ]
        [ HH.h2_ (printPath s) -- TODO Display path here with links
        , HH.div_ (map renderDeckPreviewTile decks)
        , HH.div_ (map renderCardPreviewTile cards)
        ]
    ]

printPath :: forall q. State -> Array (H.ComponentHTML Action q Aff)
printPath s =
  intercalate [HH.text " > "] (map printDeckInPath $ reverse s.path)

printDeckInPath :: forall q. Deck -> Array (H.ComponentHTML Action q Aff)
printDeckInPath d@(MakeDeck {name, decks, cards}) =
  [HH.text name]


renderDeckPreviewBar :: forall a q. Deck -> H.ComponentHTML a q Aff
renderDeckPreviewBar d@(MakeDeck {name, decks, cards}) =
  HH.div [ HP.class_ (HC.ClassName "previewBar") ]
    [
      HH.h2_ [ HH.text name ]
    ]

renderCardPreviewTile :: forall a q. Card -> H.ComponentHTML a q Aff
renderCardPreviewTile c@(MakeCard {front, back}) =
  HH.div [ HP.class_ (HC.ClassName "card") ]
    [ HH.div [ HP.class_ (HC.ClassName "cardSide") ]
        [
          HH.p_ [ HH.text front ]
        ]
      , HH.div [ HP.class_ (HC.ClassName "cardSide") ]
        [
          HH.p_ [ HH.text "--------------" ]
        ]
      , HH.div [ HP.class_ (HC.ClassName "cardSide") ]
        [
          HH.p_ [ HH.text back ]
        ]
    ]

handle :: forall q o. Action -> H.HalogenM State Action q o Aff Unit
handle a = case a of
  Init -> modify_ (_ { path = [homeDeck] })
  LaunchDeck d -> modify_ (\s -> s { path = cons d s.path })

data Card = MakeCard { front :: String, back :: String }
data Deck = MakeDeck { name :: String, decks :: Array Deck, cards :: Array Card}

germanDeck :: Deck
germanDeck = MakeDeck {
  name: "German"
  , decks: []
  , cards: [ MakeCard { front: "neutral singular, genitive", back: "des" }
            , MakeCard { front: "plural, genitive", back: "der" }
           ]
}

frenchDeck :: Deck
frenchDeck = MakeDeck {
  name: "French"
  , decks: []
  , cards: [ MakeCard { front: "chat", back: "cat" }
            , MakeCard { front: "chien", back: "dog" }
            , MakeCard { front: "canard", back: "duck" }
            , MakeCard { front: "pieuvre", back: "octopus" }
           ]
}

homeDeck :: Deck
homeDeck = MakeDeck {
  name: "~"
  , decks: [ frenchDeck, germanDeck ]
  , cards: [ MakeCard { front: "What is the answer to the universe?", back: "42" } ]
}

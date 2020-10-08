module Main where

import Prelude
import Effect (Effect)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Sparep as Sparep

main :: Effect Unit
main =
  runHalogenAff
    $ do
        body <- awaitBody
        void $ runUI Sparep.component unit body

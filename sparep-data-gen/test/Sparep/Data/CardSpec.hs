{-# LANGUAGE TypeApplications #-}

module Sparep.Data.CardSpec
  ( spec,
  )
where

import Sparep.Data.Card
import Sparep.Data.Card.Gen ()
import Test.Hspec
import Test.Validity

spec :: Spec
spec =
  genValidSpec @Card

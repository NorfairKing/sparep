{-# LANGUAGE TypeApplications #-}

module Sparep.Data.CardSpec
  ( spec,
  )
where

import Sparep.Data.Card
import Sparep.Data.Card.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec =
  genValidSpec @Card

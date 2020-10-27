{-# LANGUAGE TypeApplications #-}

module Sparep.Data.RepetitionSpec
  ( spec,
  )
where

import Sparep.Data.Repetition
import Sparep.Data.Repetition.Gen ()
import Test.Hspec
import Test.Validity

spec :: Spec
spec =
  genValidSpec @Repetition

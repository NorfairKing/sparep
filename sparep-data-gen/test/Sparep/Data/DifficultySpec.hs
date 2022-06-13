{-# LANGUAGE TypeApplications #-}

module Sparep.Data.DifficultySpec
  ( spec,
  )
where

import Sparep.Data.Difficulty
import Sparep.Data.Difficulty.Gen ()
import Test.Hspec
import Test.Validity
import Test.Validity.Persist

spec :: Spec
spec = do
  genValidSpec @Difficulty
  persistSpec @Difficulty

{-# LANGUAGE TypeApplications #-}

module Sparep.Data.DifficultySpec
  ( spec,
  )
where

import Sparep.Data.Difficulty
import Sparep.Data.Difficulty.Gen ()
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Persist

spec :: Spec
spec = do
  genValidSpec @Difficulty
  persistSpec @Difficulty

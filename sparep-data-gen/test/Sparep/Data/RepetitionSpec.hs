{-# LANGUAGE TypeApplications #-}

module Sparep.Data.RepetitionSpec
  ( spec,
  )
where

import Sparep.Data.Repetition
import Sparep.Data.Repetition.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec =
  genValidSpec @Repetition

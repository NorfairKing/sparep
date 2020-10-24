{-# LANGUAGE TypeApplications #-}

module Sparep.Data.StudyUnitSpec
  ( spec,
  )
where

import Sparep.Data.StudyUnit
import Sparep.Data.StudyUnit.Gen ()
import Test.Hspec
import Test.Validity

spec :: Spec
spec =
  genValidSpec @StudyUnit

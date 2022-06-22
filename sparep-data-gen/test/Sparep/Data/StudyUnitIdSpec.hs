{-# LANGUAGE TypeApplications #-}

module Sparep.Data.StudyUnitIdSpec
  ( spec,
  )
where

import Sparep.Data.StudyUnit.Gen ()
import Sparep.Data.StudyUnitId
import Sparep.Data.StudyUnitId.Gen ()
import Test.Hspec
import Test.Validity
import Test.Validity.Persist

spec :: Spec
spec = do
  genValidSpec @StudyUnitId
  persistSpec @StudyUnitId
  describe "hashStudyUnit" $
    it "produces valid card ids" $
      producesValid hashStudyUnit

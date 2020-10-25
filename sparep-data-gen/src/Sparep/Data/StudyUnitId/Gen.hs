{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sparep.Data.StudyUnitId.Gen where

import Data.GenValidity
import Sparep.Data.Card.Gen ()
import Sparep.Data.StudyUnit.Gen ()
import Sparep.Data.StudyUnitId

instance GenValid StudyUnitId where
  genValid = hashStudyUnit <$> genValid
  shrinkValid _ = []

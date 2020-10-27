{-# LANGUAGE DeriveGeneric #-}

module Sparep.Data.StudyUnit where

import Data.Validity
import GHC.Generics (Generic)
import Sparep.Data.Card
import Sparep.Data.FillExercise

data StudyUnit
  = CardUnit !Card
  | FillExerciseUnit !FillExercise
  deriving (Show, Eq, Generic)

instance Validity StudyUnit

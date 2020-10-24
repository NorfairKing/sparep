{-# LANGUAGE DeriveGeneric #-}

module Sparep.Data.StudyUnit where

import Data.Validity
import GHC.Generics (Generic)
import Sparep.Data.Card

data StudyUnit = CardUnit Card
  deriving (Show, Eq, Generic)

instance Validity StudyUnit

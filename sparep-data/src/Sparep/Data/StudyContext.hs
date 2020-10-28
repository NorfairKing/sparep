{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Sparep.Data.StudyContext where

import Data.Validity
import GHC.Generics (Generic)

-- Context that is gathered when determining which units to study.
data StudyContext a
  = StudyContext
      { studyContextUnit :: a,
        studyContextNew :: Bool
      }
  deriving (Show, Eq, Generic, Functor)

instance Validity a => Validity (StudyContext a)

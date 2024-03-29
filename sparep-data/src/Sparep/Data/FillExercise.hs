{-# LANGUAGE DeriveGeneric #-}

module Sparep.Data.FillExercise where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text
import Data.Validity
import Data.Validity.ByteString ()
import Data.Validity.Path ()
import Data.Validity.Text ()
import GHC.Generics (Generic)

-- | A fillExercise is a piece of text with holes for the user to type text.
newtype FillExercise = FillExercise
  { fillExerciseSequence :: FillExerciseSequence
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity FillExercise

type FillExerciseSequence = NonEmpty FillExercisePart

data FillExercisePart
  = LitPart !Text
  | FillPart !Text
  deriving (Show, Eq, Ord, Generic)

instance Validity FillExercisePart

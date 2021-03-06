{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sparep.Data.Card.Gen where

import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import Sparep.Data.Card
import Sparep.Data.Instructions.Gen ()

instance GenValid Card where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid CardSide where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

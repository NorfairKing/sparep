{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sparep.Card.Gen where

import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Containers ()
import Data.GenValidity.Text ()
import Sparep.Card

instance GenValid Deck where
  genValid = genValidStructurallyWithoutExtraChecking

  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid CardDef where
  genValid = genValidStructurallyWithoutExtraChecking

  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid CardFrontBackDef where
  genValid = genValidStructurallyWithoutExtraChecking

  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid CardManySidedDef where
  genValid = genValidStructurallyWithoutExtraChecking

  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Instructions where
  genValid = genValidStructurallyWithoutExtraChecking

  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Card where
  genValid = genValidStructurallyWithoutExtraChecking

  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid CardId where
  genValid = hashCard <$> genValid

  shrinkValid _ = []

instance GenValid Difficulty where
  genValid = genValidStructurallyWithoutExtraChecking

  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

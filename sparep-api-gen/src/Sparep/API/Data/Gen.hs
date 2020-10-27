{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sparep.API.Data.Gen where

import Data.GenValidity
import Data.GenValidity.Appendful ()
import Data.GenValidity.Text ()
import Sparep.API.Data
import Sparep.API.Server.Data.Gen ()

instance GenValid RegistrationForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid LoginForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

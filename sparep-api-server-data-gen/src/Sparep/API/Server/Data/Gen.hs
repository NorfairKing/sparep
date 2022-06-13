{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sparep.API.Server.Data.Gen where

import Control.Monad
import qualified Data.ByteString as SB
import Data.GenValidity
import Data.GenValidity.Persist ()
import Data.GenValidity.Text ()
import Data.Password.Bcrypt
import Sparep.API.Server.Data
import Sparep.Data.Gen ()
import Test.QuickCheck

instance GenValid (Salt Bcrypt) where
  genValid = Salt <$> (SB.pack <$> replicateM 32 (choose (0, 255)))
  shrinkValid _ = [] -- No use

instance GenValid Password where
  genValid = mkPassword <$> genValid
  shrinkValid _ = [] -- No use

instance GenValid (PasswordHash Bcrypt) where
  genValid = hashPasswordWithSalt <$> genValid <*> genValid <*> genValid

  shrinkValid _ = [] -- No use

instance GenValid Username

instance GenValid User

instance GenValid ServerRepetition

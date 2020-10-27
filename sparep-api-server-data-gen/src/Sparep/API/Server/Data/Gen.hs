{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sparep.API.Server.Data.Gen where

import Control.Monad
import qualified Data.ByteString as SB
import Data.GenValidity
import Data.GenValidity.Persist ()
import Data.GenValidity.Text ()
import Data.Password
import Sparep.API.Server.Data
import Sparep.Data.Gen ()
import Test.QuickCheck

instance GenValid Salt where
  genValid = Salt <$> (SB.pack <$> replicateM 32 (choose (0, 255)))
  shrinkValid _ = [] -- No use

instance GenValid Pass where
  genValid = mkPass <$> genValid
  shrinkValid _ = [] -- No use

instance GenValid PassHash where
  genValid = hashPassWithSalt <$> genValid <*> (mkPass <$> genValid)
  shrinkValid _ = [] -- No use

instance GenValid Username where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid User where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid ServerRepetition where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

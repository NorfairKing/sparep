{-# LANGUAGE TypeApplications #-}

module Sparep.DataSpec
  ( spec,
  )
where

import Sparep.Data
import Sparep.Data.Gen ()
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  genValidSpec @RegistrationForm
  genValidSpec @LoginForm
  genValidSpec @Username

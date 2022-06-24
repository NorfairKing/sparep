{-# LANGUAGE TypeApplications #-}

module Sparep.API.DataSpec
  ( spec,
  )
where

import Sparep.API.Data
import Sparep.API.Data.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @RegistrationForm
  genValidSpec @LoginForm

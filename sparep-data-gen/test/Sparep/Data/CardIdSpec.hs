{-# LANGUAGE TypeApplications #-}

module Sparep.Data.CardIdSpec
  ( spec,
  )
where

import Sparep.Data.CardId
import Sparep.Data.CardId.Gen ()
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  genValidSpec @CardId

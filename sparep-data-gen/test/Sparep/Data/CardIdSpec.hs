{-# LANGUAGE TypeApplications #-}

module Sparep.Data.CardIdSpec
  ( spec,
  )
where

import Sparep.Data.CardId
import Sparep.Data.CardId.Gen ()
import Test.Hspec
import Test.Validity
import Test.Validity.Persist

spec :: Spec
spec = do
  genValidSpec @CardId
  persistSpecOnValid @CardId
  describe "hashCard"
    $ it "produces valid card ids"
    $ producesValidsOnValids hashCard

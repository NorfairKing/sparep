{-# LANGUAGE TypeApplications #-}

module Sparep.CardSpec
  ( spec,
  )
where

import Sparep.Card
import Sparep.Card.Gen ()
import Test.Hspec
import Test.Validity
import Test.Validity.Persist

spec :: Spec
spec = do
  genValidSpec @CardId
  genValidSpec @Card
  genValidSpec @Difficulty
  describe "resolveCardDefs"
    $ it "produce valid cards"
    $ producesValidsOnValids resolveDeck
  describe "resolveCardDef"
    $ it "produce valid cards"
    $ producesValidsOnValids3 resolveCardDef
  describe "hashCard"
    $ it "produces valid card ids"
    $ producesValidsOnValids hashCard
  persistSpecOnValid @CardId
  persistSpecOnValid @Difficulty

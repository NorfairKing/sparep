module Sparep.API.Server.Handler.SyncSpec (spec) where

import Sparep.API
import Sparep.API.Data.Gen ()
import Sparep.API.Server.TestUtils
import Sparep.Client
import Sparep.Data.Gen ()
import Test.Hspec
import Test.Validity

spec :: Spec
spec = serverSpec
  $ describe "PostSync"
  $ it "does not crash"
  $ \cenv -> forAllValid $ \req -> withAnyNewUser cenv $ \token -> do
    resp <- testClientOrErr cenv $ postSync sparepClient token req
    shouldBeValid resp

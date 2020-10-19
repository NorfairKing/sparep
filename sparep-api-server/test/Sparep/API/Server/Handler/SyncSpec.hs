module Sparep.API.Server.Handler.SyncSpec (spec) where

import Network.HTTP.Types as HTTP
import Sparep.API
import Sparep.API.Data
import Sparep.API.Data.Gen ()
import Sparep.API.Server.TestUtils
import Sparep.Client
import Sparep.Data
import Sparep.Data.Gen ()
import Test.Hspec
import Test.QuickCheck
import Test.Validity

spec :: Spec
spec = serverSpec $ do
  describe "PostSync"
    $ it "does not crash"
    $ \cenv ->
      forAllValid $ \req -> do
        let token = undefined
        resp <- testClientOrErr cenv $ postSync sparepClient token req
        shouldBeValid resp

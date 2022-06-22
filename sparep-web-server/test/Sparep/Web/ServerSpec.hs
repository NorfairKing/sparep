module Sparep.Web.ServerSpec (spec) where

import Control.Monad.Logger
import Sparep.Web.Server
import Test.Hspec
import Yesod.Test

spec :: Spec
spec =
  withSparepWebServer $
    describe "HomeR" $
      it "Shows a 200" $
        do
          get HomeR
          statusIs 200

withSparepWebServer :: SpecWith (TestApp App) -> Spec
withSparepWebServer = around $ \func -> do
  let app =
        App
          { appGoogleSearchConsoleVerification = Nothing,
            appGoogleAnalyticsTracking = Nothing,
            appLogLevel = LevelWarn
          }
  func $ testApp app id

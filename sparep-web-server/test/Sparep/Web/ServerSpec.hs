module Sparep.Web.ServerSpec (spec) where

import Control.Monad.Logger
import Sparep.Web.Server
import Test.Syd
import Test.Syd.Yesod

spec :: Spec
spec =
  withSparepWebServer $
    describe "HomeR" $
      yit "Shows a 200" $ do
        get HomeR
        statusIs 200

withSparepWebServer :: YesodSpec App -> Spec
withSparepWebServer = yesodSpecWithSiteGenerator $ do
  pure
    App
      { appGoogleSearchConsoleVerification = Nothing,
        appGoogleAnalyticsTracking = Nothing,
        appLogLevel = LevelWarn
      }

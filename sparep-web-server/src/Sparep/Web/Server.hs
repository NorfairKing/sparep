{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Sparep.Web.Server where

import Control.Monad
import Control.Monad.Logger
import Data.Text (Text)
import Sparep.Web.Server.Constants
import Sparep.Web.Server.OptParse
import Sparep.Web.Server.Widget
import Text.Hamlet
import Yesod

data App
  = App
      { appLogLevel :: LogLevel,
        appGoogleAnalyticsTracking :: Maybe Text,
        appGoogleSearchConsoleVerification :: Maybe Text
      }

mkYesod
  "App"
  [parseRoutes|
/ HomeR GET
|]

instance Yesod App where
  shouldLogIO app _ ll = pure $ ll >= appLogLevel app

  defaultLayout widget = do
    app <- getYesod
    pageContent <- widgetToPageContent $ do
      addStylesheetRemote "https://cdn.jsdelivr.net/npm/bulma@0.9.1/css/bulma.min.css"
      $(widgetFile "default-body")
    withUrlRenderer $(hamletFile "templates/default-page.hamlet")

getHomeR :: Handler Html
getHomeR = defaultLayout $(widgetFile "home")

sparepWebServer :: IO ()
sparepWebServer = do
  Instructions (DispatchServe ss@ServeSettings {..}) Settings <- getInstructions
  when development $ print ss
  warp serveSetPort $
    App
      { appLogLevel = serveSetLogLevel,
        appGoogleAnalyticsTracking = serveSetGoogleAnalyticsTracking,
        appGoogleSearchConsoleVerification = serveSetGoogleSearchConsoleVerification
      }

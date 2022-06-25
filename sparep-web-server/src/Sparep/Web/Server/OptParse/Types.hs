{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Sparep.Web.Server.OptParse.Types where

import Autodocodec
import Control.Monad.Logger
import Data.Text (Text)
import Data.Yaml as Yaml (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Text.Read

data Flags = Flags
  { flagConfigFile :: !(Maybe FilePath),
    flagLogLevel :: !(Maybe LogLevel),
    flagPort :: !(Maybe Int),
    flagGoogleAnalyticsTracking :: !(Maybe String),
    flagGoogleSearchConsoleVerification :: !(Maybe String)
  }
  deriving (Show, Eq, Generic)

data Environment = Environment
  { envConfigFile :: !(Maybe FilePath),
    envLogLevel :: !(Maybe LogLevel),
    envPort :: !(Maybe Int),
    envGoogleAnalyticsTracking :: !(Maybe String),
    envGoogleSearchConsoleVerification :: !(Maybe String)
  }
  deriving (Show, Eq, Generic)

data Configuration = Configuration
  { confLogLevel :: !(Maybe LogLevel),
    confPort :: !(Maybe Int),
    confGoogleAnalyticsTracking :: !(Maybe String),
    confGoogleSearchConsoleVerification :: !(Maybe String)
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Configuration)

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> optionalField "log-level" "The minimal severity for log messages" .= confLogLevel
        <*> optionalField "port" "The port on which to serve web requests" .= confPort
        <*> optionalField "google-analytics-tracking" "The google analytics tracking code" .= confGoogleAnalyticsTracking
        <*> optionalField "google-search-console-verification" "The google search console verification code" .= confGoogleSearchConsoleVerification

instance HasCodec LogLevel where
  codec =
    stringConstCodec
      [ (LevelDebug, "Debug"),
        (LevelInfo, "Info"),
        (LevelWarn, "Warn"),
        (LevelError, "Error")
      ]

data Settings = Settings
  { setLogLevel :: !LogLevel,
    setPort :: !Int,
    setGoogleAnalyticsTracking :: !(Maybe Text),
    setGoogleSearchConsoleVerification :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

parseLogLevel :: String -> Maybe LogLevel
parseLogLevel s = readMaybe $ "Level" <> s

renderLogLevel :: LogLevel -> String
renderLogLevel = drop 5 . show

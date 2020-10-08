{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Sparep.Web.Server.OptParse.Types where

import Control.Monad.Logger
import Data.Text (Text)
import Data.Yaml as Yaml
import GHC.Generics (Generic)
import Text.Read
import YamlParse.Applicative

data Arguments
  = Arguments Command Flags
  deriving (Show, Eq)

data Instructions
  = Instructions Dispatch Settings

newtype Command
  = CommandServe ServeFlags
  deriving (Show, Eq)

data ServeFlags
  = ServeFlags
      { serveFlagLogLevel :: !(Maybe LogLevel),
        serveFlagPort :: !(Maybe Int),
        serveFlagGoogleAnalyticsTracking :: !(Maybe String),
        serveFlagGoogleSearchConsoleVerification :: !(Maybe String)
      }
  deriving (Show, Eq)

data Flags
  = Flags
      { flagConfigFile :: !(Maybe FilePath)
      }
  deriving (Show, Eq, Generic)

data Environment
  = Environment
      { envConfigFile :: !(Maybe FilePath),
        envLogLevel :: !(Maybe LogLevel),
        envPort :: !(Maybe Int),
        envGoogleAnalyticsTracking :: !(Maybe String),
        envGoogleSearchConsoleVerification :: !(Maybe String)
      }
  deriving (Show, Eq, Generic)

data Configuration
  = Configuration
      { confLogLevel :: !(Maybe LogLevel),
        confPort :: !(Maybe Int),
        confGoogleAnalyticsTracking :: !(Maybe String),
        confGoogleSearchConsoleVerification :: !(Maybe String)
      }
  deriving (Show, Eq, Generic)

instance FromJSON Configuration where
  parseJSON = viaYamlSchema

instance YamlSchema Configuration where
  yamlSchema =
    objectParser "Configuration" $
      Configuration
        <$> optionalFieldWith "log-level" "The minimal severity for log messages" (maybeParser parseLogLevel yamlSchema)
        <*> optionalField "port" "The port on which to serve web requests"
        <*> optionalField "google-analytics-tracking" "The google analytics tracking code"
        <*> optionalField "google-search-console-verification" "The google search console verification code"

newtype Dispatch
  = DispatchServe ServeSettings
  deriving (Show, Eq, Generic)

data ServeSettings
  = ServeSettings
      { serveSetLogLevel :: !LogLevel,
        serveSetPort :: !Int,
        serveSetGoogleAnalyticsTracking :: !(Maybe Text),
        serveSetGoogleSearchConsoleVerification :: !(Maybe Text)
      }
  deriving (Show, Eq, Generic)

data Settings
  = Settings
  deriving (Show, Eq, Generic)

parseLogLevel :: String -> Maybe LogLevel
parseLogLevel s = readMaybe $ "Level" <> s

renderLogLevel :: LogLevel -> String
renderLogLevel = drop 5 . show

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Sparep.API.Data where

import Data.Aeson
import qualified Data.Appendful as Appendful
import Data.Int
import qualified Data.Text as T
import Data.Text (Text)
import Data.Validity
import Data.Validity.Text ()
import Database.Persist
import Database.Persist.Sql
import Servant.API.Generic
import Servant.Auth.Server
import Sparep.Data

data RegistrationForm
  = RegistrationForm
      { registrationFormUsername :: Username,
        registrationFormPassword :: Text
      }
  deriving (Show, Eq, Ord, Generic)

instance Validity RegistrationForm

instance ToJSON RegistrationForm where
  toJSON RegistrationForm {..} =
    object
      [ "name" .= registrationFormUsername,
        "password" .= registrationFormPassword
      ]

instance FromJSON RegistrationForm where
  parseJSON =
    withObject "RegistrationForm" $ \o ->
      RegistrationForm <$> o .: "name" <*> o .: "password"

data LoginForm
  = LoginForm
      { loginFormUsername :: Username,
        loginFormPassword :: Text
      }
  deriving (Show, Eq, Ord, Generic)

instance Validity LoginForm

instance FromJSON LoginForm where
  parseJSON = withObject "LoginForm" $ \o ->
    LoginForm <$> o .: "username" <*> o .: "password"

instance ToJSON LoginForm where
  toJSON LoginForm {..} =
    object
      [ "username" .= loginFormUsername,
        "password" .= loginFormPassword
      ]

data AuthCookie
  = AuthCookie
      { authCookieUsername :: Username
      }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON AuthCookie

instance ToJSON AuthCookie

instance FromJWT AuthCookie

instance ToJWT AuthCookie

newtype Username
  = Username
      { usernameText :: Text
      }
  deriving (Show, Eq, Ord, Generic, FromJSONKey, ToJSONKey, FromJSON, ToJSON)

instance Validity Username where
  validate (Username t) =
    mconcat
      [ check (not (T.null t)) "The username is not empty.",
        check (T.length t >= 3) "The username is at least three characters long."
      ]

instance PersistField Username where
  toPersistValue (Username t) = PersistText t
  fromPersistValue (PersistText t) =
    case parseUsername t of
      Nothing -> Left "Text isn't a valid username"
      Just un -> Right un
  fromPersistValue _ = Left "Not text"

instance PersistFieldSql Username where
  sqlType _ = SqlString

parseUsername :: Text -> Maybe Username
parseUsername = constructValid . Username

-- FIXME The client Id should be a 'RepititionID' but then we need to rearrange some dependencies
-- FIXME The server Id should be a 'RepititionID' as well but then we need to rearrange even more dependencies
type SyncRequest = Appendful.SyncRequest Int64 Int64 Repetition

type SyncResponse = Appendful.SyncResponse Int64 Int64 Repetition

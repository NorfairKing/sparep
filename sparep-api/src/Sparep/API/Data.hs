{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sparep.API.Data where

import Autodocodec
import Data.Aeson (FromJSON, FromJSONKey (..), ToJSON, ToJSONKey (..))
import qualified Data.Appendful as Appendful
import Data.Functor.Contravariant
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import Database.Persist
import Database.Persist.Sql
import Servant.API.Generic
import Servant.Auth.Server
import Sparep.API.Server.Data
import Sparep.Client.Data
import Sparep.Data

data RegistrationForm = RegistrationForm
  { registrationFormUsername :: Username,
    registrationFormPassword :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec RegistrationForm)

instance Validity RegistrationForm where
  validate rf@RegistrationForm {..} =
    mconcat
      [ genericValidate rf,
        declare "The password is nonempty" $ not $ T.null registrationFormPassword
      ]

instance HasCodec RegistrationForm where
  codec =
    object "RegistrationForm" $
      RegistrationForm
        <$> requiredField "username" "user name" .= registrationFormUsername
        <*> requiredField "password" "password" .= registrationFormPassword

data LoginForm = LoginForm
  { loginFormUsername :: Username,
    loginFormPassword :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec LoginForm)

instance Validity LoginForm

instance HasCodec LoginForm where
  codec =
    object "LoginForm" $
      LoginForm
        <$> requiredField "username" "user name" .= loginFormUsername
        <*> requiredField "password" "password" .= loginFormPassword

data AuthCookie = AuthCookie
  { authCookieUsername :: Username
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON AuthCookie

instance ToJSON AuthCookie

instance FromJWT AuthCookie

instance ToJWT AuthCookie

type SyncRequest = Appendful.SyncRequest ClientRepetitionId ServerRepetitionId Repetition

type SyncResponse = Appendful.SyncResponse ClientRepetitionId ServerRepetitionId Repetition

instance ToBackendKey SqlBackend a => HasCodec (Key a) where
  codec = dimapCodec toSqlKey fromSqlKey codec

instance (PersistEntity a, ToBackendKey SqlBackend a) => ToJSONKey (Key a) where
  toJSONKey = contramap fromSqlKey toJSONKey

instance (PersistEntity a, ToBackendKey SqlBackend a) => FromJSONKey (Key a) where
  fromJSONKey = toSqlKey <$> fromJSONKey

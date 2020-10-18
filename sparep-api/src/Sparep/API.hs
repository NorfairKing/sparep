{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Sparep.API where

import Data.Proxy
import Data.Text (Text)
import Data.Validity.Text ()
import Servant.API
import Servant.API.Generic
import Servant.Auth
import Sparep.Data

sparepAPI :: Proxy SparepAPI
sparepAPI = Proxy

type SparepAPI = ToServantApi SparepRoutes

data SparepRoutes route
  = SparepRoutes
      { postRegister :: !(route :- PostRegister),
        postLogin :: !(route :- PostLogin),
        getGreeting :: !(route :- GetGreeting)
      }
  deriving (Generic)

type PostRegister =
  "register"
    :> ReqBody '[JSON] RegistrationForm
    :> Post '[JSON] NoContent

type PostLogin =
  "login"
    :> ReqBody '[JSON] LoginForm
    :> PostNoContent '[JSON] (Headers '[Header "Set-Cookie" Text] NoContent)

type ProtectAPI = Auth '[JWT] AuthCookie

type GetGreeting =
  ProtectAPI
    :> "greet"
    :> QueryParam "greeting" Text
    :> Get '[JSON] Text

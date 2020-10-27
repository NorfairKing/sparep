{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Sparep.API.Server.Data.Username where

import Data.Aeson
import qualified Data.Text as T
import Data.Text (Text)
import Data.Validity
import Data.Validity.Text ()
import Database.Persist
import Database.Persist.Sql
import GHC.Generics (Generic)
import YamlParse.Applicative

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

instance YamlSchema Username where
  yamlSchema = eitherParser parseUsernameOrErr yamlSchema

parseUsername :: Text -> Maybe Username
parseUsername = constructValid . Username

parseUsernameOrErr :: Text -> Either String Username
parseUsernameOrErr = prettyValidate . Username

{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sparep.API.Server.Data.Password
  ( module Data.Password.Bcrypt,
  )
where

import Data.Password.Bcrypt
import Data.Validity
import Data.Validity.Text ()

instance Validity Password where
  validate = validate . unsafeShowPassword

instance Validity (PasswordHash Bcrypt) where
  validate = trivialValidation

instance Validity (Salt Bcrypt) where
  validate = trivialValidation

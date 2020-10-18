{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Sparep.API.Server.Handler.Greet where

import Data.Maybe
import Sparep.API.Server.Handler.Import

handleGetGreeting :: AuthCookie -> Maybe Text -> H Text
handleGetGreeting AuthCookie {..} greeting = pure $ fromMaybe "Hello" greeting <> " " <> usernameText authCookieUsername

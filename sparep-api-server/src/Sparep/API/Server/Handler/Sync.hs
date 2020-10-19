{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Sparep.API.Server.Handler.Sync where

import Data.Maybe
import Sparep.API.Server.Handler.Import

handlePostSync :: AuthCookie -> SyncRequest -> H SyncResponse
handlePostSync AuthCookie {..} = undefined

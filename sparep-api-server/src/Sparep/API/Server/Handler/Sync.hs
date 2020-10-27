{-# LANGUAGE RecordWildCards #-}

module Sparep.API.Server.Handler.Sync where

import Data.Appendful.Persistent
import Sparep.API.Server.Handler.Import

handlePostSync :: AuthCookie -> SyncRequest -> H SyncResponse
handlePostSync AuthCookie {..} sr = withUser authCookieUsername $ \(Entity uid _) ->
  runDB $ serverProcessSyncQuery [ServerRepetitionUser ==. uid] serverMakeRepetition (makeServerRepetition uid) sr

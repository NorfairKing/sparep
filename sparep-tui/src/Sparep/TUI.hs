{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Sparep.TUI
  ( sparepTUI,
  )
where

import Brick.AttrMap
import Brick.BChan
import Brick.Main
import Brick.Util
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Graphics.Vty (defaultConfig, mkVty)
import Graphics.Vty.Attributes
import Path
import Path.IO
import Sparep.Client.Data
import Sparep.Data
import Sparep.TUI.Draw
import Sparep.TUI.Handle
import Sparep.TUI.OptParse
import Sparep.TUI.OptParse.Types
import Sparep.TUI.Repetition
import Sparep.TUI.State
import System.Exit
import System.FileLock

sparepTUI :: IO ()
sparepTUI = do
  Settings {..} <- getSettings
  ensureDir $ parent setRepetitionDb
  let dbFile = fromAbsFile setRepetitionDb
  let lockFilePath = dbFile ++ ".lock"
  mLocked <- withTryFileLock lockFilePath Exclusive $ \_ ->
    runNoLoggingT
      $ withSqlitePool (T.pack dbFile) 1
      $ \pool -> do
        runSqlPool migrateSparep pool
        liftIO $ do
          qChan <- newBChan 1000
          rChan <- newBChan 1000
          initialState <- buildInitialState qChan setDecks
          let vtyBuilder = mkVty defaultConfig
          firstVty <- vtyBuilder
          Right endState <-
            race -- Race works because the dbworker never stops
              (runReaderT (dbWorker qChan rChan) pool)
              (customMain firstVty vtyBuilder (Just rChan) (tuiApp qChan) initialState)
          case endState of
            StateStudy ss -> runSqlPool (insertMany_ (studyStateRepetitions ss)) pool
            _ -> pure ()
  case mLocked of
    Just () -> pure () -- Everything went file
    Nothing -> die "Unable to lock repetitions database."

migrateSparep :: MonadIO m => SqlPersistT m ()
migrateSparep = do
  runMigration clientMigration
  migrateRepetition

migrateRepetition :: MonadIO m => SqlPersistT m ()
migrateRepetition = do
  rs <- selectList [] []
  forM_ rs $ \(Entity rid ClientRepetition {..}) ->
    update
      rid
      [ ClientRepetitionUnit =. clientRepetitionUnit,
        ClientRepetitionDifficulty =. clientRepetitionDifficulty,
        ClientRepetitionTimestamp =. clientRepetitionTimestamp
      ] -- To make sure that it goes through a reading and writing cycle

type DB a = ReaderT ConnectionPool IO a

dbWorker :: BChan Query -> BChan Response -> DB ()
dbWorker qChan rChan = forever $ do
  query <- liftIO $ readBChan qChan
  response <- case query of
    QueryGetDeckSelection d -> do
      cs <- resolveRootedDeck d
      runDB $ ResponseGetDeckSelection d <$> generateStudySelection cs
    QueryGetDecksSelection ds -> do
      cs <- concat <$> mapM resolveRootedDeck ds
      runDB $ ResponseGetDecksSelection <$> generateStudySelection cs
    QueryGetStudyUnitDates c -> runDB $ ResponseGetStudyUnitDates c <$> getStudyUnitDates c
    QueryGetStudyUnits ds w -> do
      cs <- concat <$> mapM resolveRootedDeck ds
      runDB $ ResponseGetStudyUnits <$> generateStudyDeck cs w
  liftIO $ writeBChan rChan response

runDB :: SqlPersistT IO a -> DB a
runDB func = do
  pool <- ask
  liftIO $ runSqlPool func pool

tuiApp :: BChan Query -> App State Response ResourceName
tuiApp qChan =
  App
    { appDraw = drawTui,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleTuiEvent qChan,
      appStartEvent = pure,
      appAttrMap =
        const $
          attrMap
            (bg brightBlack)
            [ (selectedAttr, fg brightWhite),
              (headingAttr, defAttr `withStyle` underline),
              (instructionsAttr, fg yellow),
              (sideAttr, fg brightWhite),
              (litPartAttr, fg brightWhite),
              (fillPartAttr, fg magenta)
            ]
    }

buildInitialState :: BChan Query -> [RootedDeck] -> IO State
buildInitialState qChan decks = do
  writeBChan qChan $ QueryGetDecksSelection decks
  pure $ StateMenu $
    MenuState
      { menuStateDecks = decks,
        menuStateSelection = Loading
      }

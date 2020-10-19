{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Sparep.TUI
  ( sparepTUI,
  )
where

import Brick.AttrMap
import Brick.BChan
import Brick.Main
import Brick.Types
import Brick.Util
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Cursor.Simple.List.NonEmpty
import Data.List
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Time
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Graphics.Vty (defaultConfig, mkVty)
import Graphics.Vty.Attributes
import Graphics.Vty.Input
import Path
import Path.IO
import Sparep.Client.Data
import Sparep.Data
import Sparep.TUI.Card
import Sparep.TUI.Draw
import Sparep.TUI.OptParse
import Sparep.TUI.OptParse.Types
import Sparep.TUI.Repetition
import Sparep.TUI.State
import System.Exit
import System.FileLock
import System.Process.Typed

sparepTUI :: IO ()
sparepTUI = do
  Settings {..} <- getSettings
  ensureDir $ parent setRepetitionDb
  let dbFile = fromAbsFile setRepetitionDb
  let lockFile = dbFile ++ ".lock"
  mLocked <- withTryFileLock lockFile Exclusive $ \fl ->
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
  runMigration migrateAll
  migrateRepetition

migrateRepetition :: MonadIO m => SqlPersistT m ()
migrateRepetition = do
  rs <- selectList [] []
  forM_ rs $ \(Entity rid ClientRepetition {..}) -> do
    update
      rid
      [ ClientRepetitionCard =. clientRepetitionCard,
        ClientRepetitionDifficulty =. clientRepetitionDifficulty,
        ClientRepetitionTimestamp =. clientRepetitionTimestamp
      ] -- To make sure that it goes through a reading and writing cycle

type DB a = ReaderT ConnectionPool IO a

data Query
  = QueryGetDeckSelection RootedDeck
  | QueryGetDecksSelection [RootedDeck]
  | QueryGetCardDates Card
  | QueryGetStudyCards [RootedDeck] Word

data Response
  = ResponseGetDeckSelection RootedDeck (Selection Card)
  | ResponseGetDecksSelection (Selection Card)
  | ResponseGetCardDates Card (Maybe (UTCTime, UTCTime))
  | ResponseGetStudyCards [Card]

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
    QueryGetCardDates c -> runDB $ ResponseGetCardDates c <$> getCardDates c
    QueryGetStudyCards ds w -> do
      cs <- concat <$> mapM resolveRootedDeck ds
      runDB $ ResponseGetStudyCards <$> generateStudyDeck cs w
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
      appAttrMap = const $ attrMap (fg brightWhite) []
    }

buildInitialState :: BChan Query -> [RootedDeck] -> IO State
buildInitialState qChan decks = do
  writeBChan qChan $ QueryGetDecksSelection decks
  pure $ StateMenu $
    MenuState
      { menuStateDecks = decks,
        menuStateSelection = Loading
      }

handleTuiEvent ::
  BChan Query -> State -> BrickEvent n Response -> EventM n (Next State)
handleTuiEvent qChan s e =
  case s of
    StateMenu ms -> handleMenuEvent qChan ms e
    StateDecks ss -> handleDecksEvent qChan ss e
    StateCards cs -> handleCardsEvent qChan cs e
    StateStudy ss -> fmap StateStudy <$> handleStudyEvent ss e

handleMenuEvent ::
  BChan Query -> MenuState -> BrickEvent n Response -> EventM n (Next State)
handleMenuEvent qChan s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt $ StateMenu s
        EvKey (KChar 'd') [] -> do
          let decks = menuStateDecks s
          forM_ decks $ \d -> liftIO $ writeBChan qChan $ QueryGetDeckSelection d
          let decksStateCursor = makeNonEmptyCursor <$> NE.nonEmpty (map (\d -> (d, Loading)) $ sortOn (deckName . rootedDeckDeck) decks)
          continue $ StateDecks DecksState {..}
        EvKey KEnter [] -> handleStudy qChan (menuStateDecks s)
        _ -> continue $ StateMenu s
    AppEvent (ResponseGetDecksSelection sel) -> do
      let s' = s {menuStateSelection = Loaded sel}
      continue $ StateMenu s'
    _ -> continue $ StateMenu s

handleDecksEvent ::
  BChan Query -> DecksState -> BrickEvent n Response -> EventM n (Next State)
handleDecksEvent qChan s e = case decksStateCursor s of
  Nothing ->
    case e of
      VtyEvent vtye ->
        case vtye of
          EvKey (KChar 'q') [] -> halt $ StateDecks s
          _ -> continue $ StateDecks s
      _ -> continue $ StateDecks s
  Just cursor ->
    case e of
      VtyEvent vtye ->
        case vtye of
          EvKey (KChar 'q') [] -> halt $ StateDecks s
          EvKey (KChar 'c') [] -> do
            let cardsStateDeck = fst (nonEmptyCursorCurrent cursor)
            cards <- resolveRootedDeck cardsStateDeck
            forM_ cards $ \c -> liftIO $ writeBChan qChan $ QueryGetCardDates c
            let cardsStateCursor = makeNonEmptyCursor <$> NE.nonEmpty (map (\c -> (c, Loading)) cards)
            continue $ StateCards $ CardsState {..}
          EvKey KEnter [] -> handleStudy qChan [fst (nonEmptyCursorCurrent cursor)]
          _ -> continue $ StateDecks s
      AppEvent (ResponseGetDeckSelection d sel) -> do
        let mCursor' = flip fmap (decksStateCursor s) $ mapNonEmptyCursor (\t@(d', _) -> if d == d' then (d', Loaded sel) else t)
        let s' = s {decksStateCursor = mCursor'}
        continue $ StateDecks s'
      _ -> continue $ StateDecks s

handleCardsEvent ::
  BChan Query -> CardsState -> BrickEvent n Response -> EventM n (Next State)
handleCardsEvent qChan s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt $ StateCards s
        EvKey KEsc [] -> halt $ StateCards s
        EvKey KEnter [] -> handleStudy qChan [cardsStateDeck s]
        _ -> continue $ StateCards s
    AppEvent (ResponseGetCardDates c dates) -> do
      let mCursor' = flip fmap (cardsStateCursor s) $ mapNonEmptyCursor (\t@(c', _) -> if c == c' then (c', Loaded dates) else t)
      let s' = s {cardsStateCursor = mCursor'}
      continue $ StateCards s'
    _ -> continue $ StateCards s

handleStudyEvent ::
  StudyState ->
  BrickEvent n Response ->
  EventM n (Next StudyState)
handleStudyEvent s e =
  case studyStateCursor s of
    Loading ->
      case e of
        VtyEvent vtye ->
          case vtye of
            EvKey (KChar 'q') [] -> halt s
            _ -> continue s
        AppEvent (ResponseGetStudyCards cs) -> continue $ s {studyStateCursor = Loaded $ makeNonEmptyCursor <$> NE.nonEmpty cs}
        _ -> continue s
    Loaded mCursor ->
      case e of
        VtyEvent vtye ->
          case mCursor of
            Nothing -> halt s
            Just cursor ->
              let tryPlay :: FrontBack -> EventM n (Next StudyState)
                  tryPlay fb = do
                    let cur = nonEmptyCursorCurrent cursor
                    let side = case fb of
                          Front -> cardFront cur
                          Back -> cardBack cur
                    case side of
                      TextSide _ -> pure ()
                      SoundSide fp _ -> runProcess_ $ setStdout nullStream $ setStderr nullStream $ proc "play" [fromAbsFile fp]
                    continue s
               in case studyStateFrontBack s of
                    Front ->
                      case vtye of
                        EvKey (KChar 'f') [] -> tryPlay Front
                        EvKey (KChar 'q') [] -> halt s
                        EvKey (KChar ' ') [] -> continue $ s {studyStateFrontBack = Back}
                        _ -> continue s
                    Back ->
                      let finishCard :: Difficulty -> EventM n (Next StudyState)
                          finishCard difficulty = do
                            let cur = nonEmptyCursorCurrent cursor
                            now <- liftIO getCurrentTime
                            let rep =
                                  ClientRepetition
                                    { clientRepetitionCard = hashCard cur,
                                      clientRepetitionDifficulty = difficulty,
                                      clientRepetitionTimestamp = now
                                    }
                            let mcursor' = nonEmptyCursorSelectNext cursor
                            let mcursor'' =
                                  -- Require re-studying incorrect cards
                                  if difficulty == CardIncorrect
                                    then Just $ case mcursor' of
                                      Nothing -> singletonNonEmptyCursor cur
                                      Just cursor' -> nonEmptyCursorAppendAtEnd cur cursor'
                                    else mcursor'
                            continue $
                              s
                                { studyStateCursor = Loaded mcursor'',
                                  studyStateFrontBack = Front,
                                  studyStateRepetitions = rep : studyStateRepetitions s
                                }
                       in case vtye of
                            EvKey (KChar 'f') [] -> tryPlay Front
                            EvKey (KChar 'b') [] -> tryPlay Back
                            EvKey (KChar 'q') [] -> halt s
                            EvKey (KChar 'i') [] -> finishCard CardIncorrect
                            EvKey (KChar 'h') [] -> finishCard CardHard
                            EvKey (KChar 'g') [] -> finishCard CardGood
                            EvKey (KChar 'e') [] -> finishCard CardEasy
                            _ -> continue s
        _ -> continue s

-- This computation may take a while, move it to a separate thread with a nice progress bar.
handleStudy :: BChan Query -> [RootedDeck] -> EventM n (Next State)
handleStudy qChan decks = do
  liftIO $ writeBChan qChan $ QueryGetStudyCards decks 25
  let studyStateRepetitions = []
  let studyStateFrontBack = Front
  let studyStateCursor = Loading
  continue $ StateStudy $ StudyState {..}

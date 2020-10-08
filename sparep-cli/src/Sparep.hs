{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Sparep
  ( sparep,
  )
where

import Brick.AttrMap
import Brick.BChan
import Brick.Main
import Brick.Types
import Brick.Util
import Control.Concurrent
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
import Graphics.Vty
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Path
import Path.IO
import Safe
import Sparep.Card
import Sparep.DB
import Sparep.Draw
import Sparep.OptParse
import Sparep.OptParse.Types
import Sparep.Repetition
import Sparep.State

sparep :: IO ()
sparep = do
  Settings {..} <- getSettings
  ensureDir $ parent setRepetitionDb
  runNoLoggingT
    $ withSqlitePool (T.pack $ fromAbsFile setRepetitionDb) 1
    $ \pool -> do
      runSqlPool (runMigration migrateAll) pool
      liftIO $ do
        initialState <- buildInitialState setDecks
        qChan <- newBChan 1000
        rChan <- newBChan 1000
        let vtyBuilder = mkVty defaultConfig
        firstVty <- vtyBuilder
        Right endState <-
          race -- Race works because the dbworker never stops
            (runReaderT (dbWorker qChan rChan) pool)
            (customMain firstVty vtyBuilder (Just rChan) (tuiApp qChan) initialState)
        case endState of
          StateStudy ss -> runSqlPool (insertMany_ (studyStateRepetitions ss)) pool
          _ -> pure ()

completeLoading :: a -> Loading a -> Loading a
completeLoading a Loading = Loaded a
completeLoading a (Loaded _) = Loaded a

type DB a = ReaderT ConnectionPool IO a

data Query
  = QueryGetDeckSelection Deck
  | QueryGetDecksSelection [Deck]
  | QueryGetCardDates Card
  | QueryGetStudyCards [Deck] Word

data Response
  = ResponseGetDeckSelection Deck (Selection Card)
  | ResponseGetDecksSelection [Deck] (Selection Card)
  | ResponseGetCardDates Card (Maybe (UTCTime, UTCTime))
  | ResponseGetStudyCards [Card]

dbWorker :: BChan Query -> BChan Response -> DB ()
dbWorker qChan rChan = forever $ do
  query <- liftIO $ readBChan qChan
  liftIO $ threadDelay 1000000
  response <- case query of
    QueryGetDeckSelection d -> runDB $ ResponseGetDeckSelection d <$> generateStudySelection (resolveDeck d)
    QueryGetDecksSelection ds -> runDB $ ResponseGetDecksSelection ds <$> generateStudySelection (concatMap resolveDeck ds)
    QueryGetCardDates c -> runDB $ ResponseGetCardDates c <$> getCardDates c
    QueryGetStudyCards ds w -> runDB $ ResponseGetStudyCards <$> generateStudyDeck (concatMap resolveDeck ds) w
  liftIO $ writeBChan rChan response

runDB :: SqlPersistT IO a -> DB a
runDB func = do
  pool <- ask
  liftIO $ runSqlPool func pool

tuiApp :: BChan Query -> App State e ResourceName
tuiApp qChan =
  App
    { appDraw = drawTui,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleTuiEvent qChan,
      appStartEvent = pure,
      appAttrMap = const $ attrMap (fg brightWhite) []
    }

buildInitialState :: [Deck] -> IO State
buildInitialState decks =
  pure $ StateMenu $ MenuState {menuStateDecks = decks}

handleTuiEvent ::
  BChan Query -> State -> BrickEvent n e -> EventM n (Next State)
handleTuiEvent qChan s e =
  case s of
    StateMenu ms -> handleMenuEvent qChan ms e
    StateDecks ss -> handleDecksEvent qChan ss e
    StateCards cs -> handleCardsEvent qChan cs e
    StateStudy ss -> fmap StateStudy <$> handleStudyEvent ss e

handleMenuEvent ::
  BChan Query -> MenuState -> BrickEvent n e -> EventM n (Next State)
handleMenuEvent qChan s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt $ StateMenu s
        EvKey (KChar 'd') [] -> do
          let decks = menuStateDecks s
          forM decks $ \d -> liftIO $ writeBChan qChan $ QueryGetDeckSelection d
          let decksStateCursor = makeNonEmptyCursor <$> NE.nonEmpty (map (\d -> (d, Loading)) decks)
          continue $ StateDecks DecksState {..}
        EvKey KEnter [] -> handleStudy qChan (menuStateDecks s)
        _ -> continue $ StateMenu s
    _ -> continue $ StateMenu s

handleDecksEvent ::
  BChan Query -> DecksState -> BrickEvent n e -> EventM n (Next State)
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
            let cards = resolveDeck cardsStateDeck
            forM cards $ \c -> liftIO $ writeBChan qChan $ QueryGetCardDates c
            let cardsStateCursor = makeNonEmptyCursor <$> NE.nonEmpty (map (\c -> (c, Loading)) cards)
            continue $ StateCards $ CardsState {..}
          EvKey KEnter [] -> handleStudy qChan [fst (nonEmptyCursorCurrent cursor)]
          _ -> continue $ StateDecks s
      _ -> continue $ StateDecks s

handleCardsEvent ::
  BChan Query -> CardsState -> BrickEvent n e -> EventM n (Next State)
handleCardsEvent qChan s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt $ StateCards s
        EvKey KEsc [] -> halt $ StateCards s
        EvKey KEnter [] -> handleStudy qChan [cardsStateDeck s]
        _ -> continue $ StateCards s
    _ -> continue $ StateCards s

handleStudyEvent ::
  StudyState ->
  BrickEvent n e ->
  EventM n (Next StudyState)
handleStudyEvent s e =
  case studyStateCursor s of
    Loading ->
      case e of
        VtyEvent vtye ->
          case vtye of
            EvKey (KChar 'q') [] -> halt s
            _ -> continue s
        _ -> continue s
    Loaded mCursor ->
      case e of
        VtyEvent vtye ->
          case mCursor of
            Nothing -> halt s
            Just cursor ->
              case studyStateFrontBack s of
                Front ->
                  case vtye of
                    EvKey (KChar 'q') [] -> halt s
                    EvKey (KChar ' ') [] -> continue $ s {studyStateFrontBack = Back}
                    _ -> continue s
                Back ->
                  let finishCard :: Difficulty -> EventM n (Next StudyState)
                      finishCard difficulty = do
                        let cur = nonEmptyCursorCurrent cursor
                        now <- liftIO getCurrentTime
                        let rep =
                              Repetition
                                { repetitionCard = hashCard cur,
                                  repetitionDifficulty = difficulty,
                                  repetitionTimestamp = now
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
                            { studyStateCursor = Loaded $ mcursor'',
                              studyStateFrontBack = Front,
                              studyStateRepetitions = rep : studyStateRepetitions s
                            }
                   in case vtye of
                        EvKey (KChar 'q') [] -> halt s
                        EvKey (KChar 'i') [] -> finishCard CardIncorrect
                        EvKey (KChar 'h') [] -> finishCard CardHard
                        EvKey (KChar 'g') [] -> finishCard CardGood
                        EvKey (KChar 'e') [] -> finishCard CardEasy
                        _ -> continue s
        _ -> continue s

-- This computation may take a while, move it to a separate thread with a nice progress bar.
handleStudy :: BChan Query -> [Deck] -> EventM n (Next State)
handleStudy qChan decks = do
  liftIO $ writeBChan qChan $ QueryGetStudyCards decks 25
  let studyStateRepetitions = []
  let studyStateFrontBack = Front
  let studyStateCursor = Loading
  continue $ StateStudy $ StudyState {..}
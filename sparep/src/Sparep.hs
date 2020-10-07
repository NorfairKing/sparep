{-# LANGUAGE RecordWildCards #-}

module Sparep
  ( sparep,
  )
where

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Util
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Cursor.Simple.List.NonEmpty
import Data.List
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Time
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
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
        void $ defaultMain (tuiApp pool) initialState

tuiApp :: ConnectionPool -> App State e ResourceName
tuiApp pool =
  App
    { appDraw = drawTui,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleTuiEvent pool,
      appStartEvent = pure,
      appAttrMap = const $ attrMap (fg brightWhite) []
    }

buildInitialState :: [Deck] -> IO State
buildInitialState decks =
  pure $ StateMenu $ MenuState {menuStateDecks = decks}

handleTuiEvent ::
  ConnectionPool -> State -> BrickEvent n e -> EventM n (Next State)
handleTuiEvent pool s e =
  case s of
    StateMenu ms -> handleMenuEvent pool ms e
    StateDecks ss -> handleDecksEvent pool ss e
    StateCards cs -> handleCardsEvent pool cs e
    StateStudy ss -> fmap StateStudy <$> handleStudyEvent pool ss e

handleMenuEvent ::
  ConnectionPool -> MenuState -> BrickEvent n e -> EventM n (Next State)
handleMenuEvent pool s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt $ StateMenu s
        EvKey (KChar 'd') [] -> do
          ds <- liftIO $ forM (menuStateDecks s) $ \d -> (,) d <$> generateStudySelection pool (resolveDeck d)
          case NE.nonEmpty ds of
            Nothing -> halt $ StateMenu s
            Just ne -> do
              let decksStateCursor = makeNonEmptyCursor ne
              continue $ StateDecks DecksState {..}
        EvKey KEnter [] -> handleStudy pool (menuStateDecks s)
        _ -> continue $ StateMenu s
    _ -> continue $ StateMenu s

handleDecksEvent ::
  ConnectionPool -> DecksState -> BrickEvent n e -> EventM n (Next State)
handleDecksEvent pool s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt $ StateDecks s
        EvKey (KChar 'c') [] -> do
          let cardsStateDeck = fst (nonEmptyCursorCurrent (decksStateCursor s))
          let cards = resolveDeck cardsStateDeck
          tups <- forM cards $ \card -> do
            let getReps = map entityVal <$> selectList [RepetitionCard ==. hashCard card] [Desc RepetitionTimestamp]
            reps <- liftIO $ runSqlPool getReps pool
            pure (card, repetitionTimestamp <$> headMay reps, nextRepititionSM2 reps)
          let cardsStateCursor = makeNonEmptyCursor <$> NE.nonEmpty (sortOn (\(_, _, z) -> z) tups)
          continue $ StateCards $ CardsState {..}
        EvKey KEnter [] -> handleStudy pool [fst (nonEmptyCursorCurrent (decksStateCursor s))]
        _ -> continue $ StateDecks s
    _ -> continue $ StateDecks s

handleCardsEvent ::
  ConnectionPool -> CardsState -> BrickEvent n e -> EventM n (Next State)
handleCardsEvent pool s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt $ StateCards s
        EvKey KEsc [] -> halt $ StateCards s
        EvKey KEnter [] -> handleStudy pool [cardsStateDeck s]
        _ -> continue $ StateCards s
    _ -> continue $ StateCards s

handleStudyEvent ::
  ConnectionPool ->
  StudyState ->
  BrickEvent n e ->
  EventM n (Next StudyState)
handleStudyEvent pool s e =
  case e of
    VtyEvent vtye ->
      case studyStateCursor s of
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
                    let query =
                          -- TODO move the querying to a separate thread
                          insert_
                            ( Repetition
                                { repetitionCard = hashCard cur,
                                  repetitionDifficulty = difficulty,
                                  repetitionTimestamp = now
                                }
                            )
                    let mcursor' = nonEmptyCursorSelectNext cursor
                    let mcursor'' =
                          -- Require re-studying incorrect cards
                          if difficulty == CardIncorrect
                            then Just $ case mcursor' of
                              Nothing -> singletonNonEmptyCursor cur
                              Just cursor' -> nonEmptyCursorAppendAtEnd cur cursor'
                            else mcursor'
                    liftIO $ runSqlPool query pool
                    continue $
                      s
                        { studyStateCursor = mcursor'',
                          studyStateFrontBack = Front
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
handleStudy :: ConnectionPool -> [Deck] -> EventM n (Next State)
handleStudy pool decks = do
  cs <-
    liftIO $
      generateStudyDeck
        pool
        (concatMap resolveDeck decks)
        25
  case NE.nonEmpty cs of
    Nothing -> do
      let studyStateCursor = Nothing
      let studyStateFrontBack = Front
      continue $ StateStudy $ StudyState {..}
    Just ne -> do
      let studyStateCursor = Just $ makeNonEmptyCursor ne
      let studyStateFrontBack = Front
      continue $ StateStudy $ StudyState {..}

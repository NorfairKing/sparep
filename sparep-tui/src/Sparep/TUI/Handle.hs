{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Sparep.TUI.Handle where

import Brick.BChan
import Brick.Main
import Brick.Types
import Control.Monad
import Control.Monad.IO.Class
import Cursor.Simple.List.NonEmpty
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Time
import Graphics.Vty.Input
import Path
import Sparep.Client.Data
import Sparep.Data
import Sparep.TUI.Repetition
import Sparep.TUI.State
import System.Process.Typed

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
        let mDo func =
              let cursor' = fromMaybe cursor $ func cursor
               in continue $ StateDecks $ s {decksStateCursor = Just cursor'}
         in case vtye of
              EvKey (KChar 'q') [] -> halt $ StateDecks s
              EvKey KUp [] -> mDo nonEmptyCursorSelectPrev
              EvKey (KChar 'k') [] -> mDo nonEmptyCursorSelectPrev
              EvKey KDown [] -> mDo nonEmptyCursorSelectNext
              EvKey (KChar 'j') [] -> mDo nonEmptyCursorSelectNext
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
      let mDo func =
            let mcursor' = case cardsStateCursor s of
                  Nothing -> Nothing
                  Just cursor -> Just $ fromMaybe cursor $ func cursor
             in continue $ StateCards $ s {cardsStateCursor = mcursor'}
          tryPlay :: FrontBack -> EventM n (Next State)
          tryPlay fb =
            case cardsStateCursor s of
              Nothing -> continue $ StateCards s
              Just cursor -> do
                let card = fst (nonEmptyCursorCurrent cursor)
                    side = case fb of
                      Front -> cardFront card
                      Back -> cardBack card
                case side of
                  TextSide _ -> pure ()
                  SoundSide fp _ -> playSoundFile fp
                continue $ StateCards s
       in case vtye of
            EvKey (KChar 'q') [] -> halt $ StateCards s
            EvKey KUp [] -> mDo nonEmptyCursorSelectPrev
            EvKey (KChar 'k') [] -> mDo nonEmptyCursorSelectPrev
            EvKey KDown [] -> mDo nonEmptyCursorSelectNext
            EvKey (KChar 'j') [] -> mDo nonEmptyCursorSelectNext
            EvKey (KChar 'f') [] -> tryPlay Front
            EvKey (KChar 'b') [] -> tryPlay Back
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
                      SoundSide fp _ -> playSoundFile fp
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
                                      clientRepetitionTimestamp = now,
                                      clientRepetitionServerId = Nothing
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

playSoundFile :: Path Abs File -> EventM n ()
playSoundFile p = runProcess_ $ setStdout nullStream $ setStderr nullStream $ proc "play" [fromAbsFile p]

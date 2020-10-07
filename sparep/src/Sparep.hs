{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Sparep
  ( sparep,
  )
where

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Core
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Cursor.Brick
import Cursor.Simple.List.NonEmpty
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Maybe
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
import Sparep.OptParse
import Sparep.OptParse.Types
import Sparep.Repetition

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

data State
  = StateMenu MenuState
  | StateDecks DecksState
  | StateCards CardsState
  | StateStudy StudyState

data MenuState
  = MenuState
      { menuStateDecks :: [Deck]
      }
  deriving (Show, Eq)

data DecksState
  = DecksState
      { decksStateCursor :: NonEmptyCursor (Deck, Selection Card)
      }
  deriving (Show, Eq)

data CardsState
  = CardsState
      { cardsStateDeck :: Deck,
        cardsStateCursor :: Maybe (NonEmptyCursor (Card, Maybe UTCTime, Maybe UTCTime))
      }
  deriving (Show, Eq)

data StudyState
  = StudyState
      { studyStateCursor :: Maybe (NonEmptyCursor Card),
        studyStateFrontBack :: FrontBack
      }
  deriving (Show, Eq)

data FrontBack
  = Front
  | Back
  deriving (Show, Eq)

data ResourceName
  = ResourceName
  deriving (Show, Eq, Ord)

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

drawTui :: State -> [Widget ResourceName]
drawTui =
  \case
    StateMenu ms -> drawMenuState ms
    StateDecks ds -> drawDecksState ds
    StateCards ds -> drawCardsState ds
    StateStudy ss -> drawStudyState ss

drawMenuState :: MenuState -> [Widget ResourceName]
drawMenuState MenuState {..} =
  [ centerLayer
      $ border
      $ padAll 1
      $ vBox
        [ str "Sparep",
          str " ",
          hBox
            [ str "Found ",
              str (show (length menuStateDecks)),
              str " decks containing"
            ],
          hBox
            [ str (show (length (concatMap deckCards menuStateDecks))),
              str " card definitions"
            ],
          hBox
            [ str "which resolve to ",
              str (show (length (concatMap resolveDeck menuStateDecks))),
              str " cards"
            ],
          str " ",
          str "Press enter to study now",
          str " ",
          str "Press d to show decks"
        ]
  ]

drawDecksState :: DecksState -> [Widget ResourceName]
drawDecksState DecksState {..} =
  [ vBox
      [ padBottom Max $
          let go (Deck {..}, Selection {..}) =
                [ txt $ fromMaybe "No Name" deckName,
                  str (show (length selectionTooSoon)),
                  str (show (length selectionReady)),
                  str (show (length selectionNew))
                ]
           in verticalNonEmptyCursorTableWithHeader go go go [str "Name", str "Done", str "Ready", str "New"] decksStateCursor,
        str "Press enter to study the selected deck",
        str "Press c to show the cards in the selected deck"
      ]
  ]

drawCardsState :: CardsState -> [Widget ResourceName]
drawCardsState CardsState {..} =
  [ vBox
      [ padBottom Max $ case cardsStateCursor of
          Nothing -> str "No cards"
          Just cursor ->
            let showTime = formatTime defaultTimeLocale "%F %R"
                go (Card {..}, mPrevTime, mNextTime) =
                  [ txt cardFront,
                    txt cardBack,
                    str (maybe "" showTime mPrevTime),
                    str (maybe "" showTime mNextTime)
                  ]
             in verticalNonEmptyCursorTableWithHeader go go go [str "Front", str "Back", str "Last Study", str "Next Study"] cursor,
        str "Press Escape to go back to the deck menu"
      ]
  ]

verticalNonEmptyCursorTableWithHeader ::
  (a -> [Widget n]) -> (a -> [Widget n]) -> (a -> [Widget n]) -> [Widget n] -> NonEmptyCursor a -> Widget n
verticalNonEmptyCursorTableWithHeader prevFunc curFunc nextFunc header =
  nonEmptyCursorWidget (\ps c ns -> drawTable $ header : (map prevFunc ps ++ [curFunc c] ++ map nextFunc ns))

drawTable :: [[Widget n]] -> Widget n
drawTable = hBox . intersperse (str " ") . map vBox . transpose

drawStudyState :: StudyState -> [Widget ResourceName]
drawStudyState StudyState {..} =
  [ case studyStateCursor of
      Nothing -> centerLayer $ str "Done"
      Just cursor ->
        let Card {..} = nonEmptyCursorCurrent cursor
         in vBox
              [ hCenterLayer
                  $ str
                  $ show (length (nonEmptyCursorNext cursor)) ++ " cards left",
                vCenterLayer $ vBox
                  $ map hCenterLayer
                  $ concat
                    [ [padLeftRight 3 $ txt ins | ins <- maybeToList cardInstructions],
                      [ padAll 1
                          $ border
                          $ vBox
                          $ concat
                            [ [padAll 1 $ txt cardFront],
                              case studyStateFrontBack of
                                Front -> []
                                Back -> [padAll 1 $ txt cardBack]
                            ],
                        padLeftRight 3 $
                          case studyStateFrontBack of
                            Front -> str "Show back: space"
                            Back -> padAll 1 $ str "Incorrect: i,  Hard: h,  Good: g,  Easy: e"
                      ]
                    ]
              ]
  ]

handleTuiEvent ::
  ConnectionPool -> State -> BrickEvent n e -> EventM n (Next State)
handleTuiEvent pool s e =
  case s of
    StateMenu ms -> handleMenuEvent pool ms e
    StateDecks ss -> handleDeckEvent pool ss e
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

handleDeckEvent ::
  ConnectionPool -> DecksState -> BrickEvent n e -> EventM n (Next State)
handleDeckEvent pool s e =
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
          let cardsStateCursor = makeNonEmptyCursor <$> NE.nonEmpty tups
          continue $ StateCards $ CardsState {..}
        EvKey KEnter [] -> handleStudy pool [fst (nonEmptyCursorCurrent (decksStateCursor s))]
        _ -> continue $ StateDecks s
    _ -> continue $ StateDecks s

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
                    case nonEmptyCursorSelectNext cursor of
                      Nothing -> halt s
                      Just cursor' -> do
                        let cursor'' =
                              -- Require re-studying incorrect cards
                              if difficulty == CardIncorrect
                                then nonEmptyCursorAppend cur cursor'
                                else cursor'
                        liftIO $ runSqlPool query pool
                        continue $
                          s
                            { studyStateCursor = Just cursor'',
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
        10
  case NE.nonEmpty cs of
    Nothing -> do
      let studyStateCursor = Nothing
      let studyStateFrontBack = Front
      continue $ StateStudy $ StudyState {..}
    Just ne -> do
      let studyStateCursor = Just $ makeNonEmptyCursor ne
      let studyStateFrontBack = Front
      continue $ StateStudy $ StudyState {..}

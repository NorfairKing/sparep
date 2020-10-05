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
import Cursor.Simple.List.NonEmpty
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
        initialState <- buildInitialState setCardDefs
        void $ defaultMain (tuiApp pool) initialState

data State
  = StateMenu MenuState
  | StateStudy StudyState

data MenuState
  = MenuState
      { menuStateCardDefs :: [CardDefs]
      }
  deriving (Show, Eq)

data StudyState
  = StudyState
      { studyStateCursor :: NonEmptyCursor Card,
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

buildInitialState :: [CardDefs] -> IO State
buildInitialState cardDefs =
  pure $ StateMenu $ MenuState {menuStateCardDefs = cardDefs}

drawTui :: State -> [Widget ResourceName]
drawTui =
  \case
    StateMenu ms -> drawMenuState ms
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
              str (show (length (concatMap cardDefsCards menuStateCardDefs))),
              str " card definitions"
            ],
          hBox
            [ str "which resolve to ",
              str (show (length (concatMap resolveCardDefs menuStateCardDefs))),
              str " cards"
            ],
          str " ",
          str "Press enter to study now"
        ]
  ]

drawStudyState :: StudyState -> [Widget ResourceName]
drawStudyState StudyState {..} =
  let Card {..} = nonEmptyCursorCurrent studyStateCursor
   in [ vBox
          [ hCenter $
              hBox
                [ str (show (length (nonEmptyCursorNext studyStateCursor))),
                  str " cards left"
                ],
            centerLayer
              $ border
              $ vBox
              $ concat
                [ [padAll 1 $ txt cardFront],
                  case studyStateFrontBack of
                    Front -> []
                    Back -> [padAll 1 $ txt cardBack]
                ],
            hCenter $
              case studyStateFrontBack of
                Front -> str "Show back: space"
                Back ->
                  hBox $
                    map
                      (padAll 1)
                      [str "Incorrect: i", str "Correct: c", str "Easy: e"]
          ]
      ]

handleTuiEvent ::
  ConnectionPool -> State -> BrickEvent n e -> EventM n (Next State)
handleTuiEvent pool s e =
  case s of
    StateMenu ms -> handleMenuEvent pool ms e
    StateStudy ss -> fmap StateStudy <$> handleStudyEvent pool ss e

handleMenuEvent ::
  ConnectionPool -> MenuState -> BrickEvent n e -> EventM n (Next State)
handleMenuEvent pool s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt $ StateMenu s
        EvKey KEnter [] ->
          -- This computation may take a while, move it to a separate thread with a nice progress bar.
          do
            cs <-
              liftIO $
                generateStudyDeck
                  pool
                  (concatMap resolveCardDefs (menuStateCardDefs s))
                  10
            case NE.nonEmpty cs of
              Nothing -> halt $ StateMenu s
              Just ne -> do
                let studyStateCursor = makeNonEmptyCursor ne
                let studyStateFrontBack = Front
                continue $ StateStudy StudyState {..}
        _ -> continue $ StateMenu s
    _ -> continue $ StateMenu s

handleStudyEvent ::
  ConnectionPool ->
  StudyState ->
  BrickEvent n e ->
  EventM n (Next StudyState)
handleStudyEvent pool s e =
  case e of
    VtyEvent vtye ->
      case studyStateFrontBack s of
        Front ->
          case vtye of
            EvKey (KChar 'q') [] -> halt s
            EvKey (KChar ' ') [] -> continue $ s {studyStateFrontBack = Back}
            _ -> continue s
        Back ->
          let finishCard :: Difficulty -> EventM n (Next StudyState)
              finishCard difficulty = do
                let cursor = studyStateCursor s
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
                    liftIO $ runSqlPool query pool
                    continue $
                      s
                        { studyStateCursor = cursor',
                          studyStateFrontBack = Front
                        }
           in case vtye of
                EvKey (KChar 'q') [] -> halt s
                EvKey (KChar 'i') [] -> finishCard CardIncorrect
                EvKey (KChar 'c') [] -> finishCard CardCorrect
                EvKey (KChar 'e') [] -> finishCard CardEasy
                _ -> continue s
    _ -> continue s

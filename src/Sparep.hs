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
import Cursor.Simple.List.NonEmpty
import qualified Data.List.NonEmpty as NE
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Path
import Path.IO
import Sparep.Card
import System.Exit
import YamlParse.Applicative

sparep :: IO ()
sparep = do
  p <- resolveFile' "/home/syd/src/german/vocab/das-geld.yaml"
  initialState <- buildInitialState p
  endState <- defaultMain tuiApp initialState
  print endState

data State
  = State
      { stateCursor :: NonEmptyCursor Card,
        stateFrontBack :: FrontBack
      }
  deriving (Show, Eq)

data FrontBack = Front | Back
  deriving (Show, Eq)

data ResourceName
  = ResourceName
  deriving (Show, Eq, Ord)

tuiApp :: App State e ResourceName
tuiApp =
  App
    { appDraw = drawTui,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleTuiEvent,
      appStartEvent = pure,
      appAttrMap = const $ attrMap (fg brightWhite) []
    }

buildInitialState :: Path Abs File -> IO State
buildInitialState cardDefsPath = do
  mcd <- readConfigFile cardDefsPath
  case mcd of
    Nothing -> die $ "File does not exist: " <> fromAbsFile cardDefsPath
    Just cds -> case NE.nonEmpty $ resolveCardDefs cds of
      Nothing -> die "No cards to study."
      Just ne -> do
        let stateCursor = makeNonEmptyCursor ne
        let stateFrontBack = Front
        pure State {..}

drawTui :: State -> [Widget ResourceName]
drawTui State {..} =
  let Card {..} = nonEmptyCursorCurrent stateCursor
   in [ vBox
          [ centerLayer $ border
              $ vBox
              $ concat
                [ [padAll 1 $ txt cardFront],
                  case stateFrontBack of
                    Front -> []
                    Back ->
                      [ padAll 1 $ txt cardBack
                      ]
                ],
            hCenter $
              case stateFrontBack of
                Front -> str "Show back: space"
                Back ->
                  hBox $ map (padAll 1) $
                    [ str "Incorrect: i",
                      str "Correct: c",
                      str "Easy: e"
                    ]
          ]
      ]

handleTuiEvent :: State -> BrickEvent n e -> EventM n (Next State)
handleTuiEvent s e =
  case e of
    VtyEvent vtye ->
      case stateFrontBack s of
        Front ->
          case vtye of
            EvKey (KChar 'q') [] -> halt s
            EvKey (KChar ' ') [] -> continue $ s {stateFrontBack = Back}
            _ -> continue s
        Back ->
          let finishCard :: Difficulty -> EventM n (Next State)
              finishCard _ = do
                let cur = stateCursor s
                case nonEmptyCursorSelectNext cur of
                  Nothing -> halt s
                  Just cur' ->
                    continue $
                      s
                        { stateCursor = cur',
                          stateFrontBack = Front
                        }
           in case vtye of
                EvKey (KChar 'q') [] -> halt s
                EvKey (KChar 'i') [] -> finishCard CardIncorrect
                EvKey (KChar 'c') [] -> finishCard CardCorrect
                EvKey (KChar 'e') [] -> finishCard CardEasy
                _ -> continue s
    _ -> continue s

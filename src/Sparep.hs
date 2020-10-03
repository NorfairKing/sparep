{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Sparep
  ( sparep,
  )
where

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Cursor.Simple.List.NonEmpty
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Yaml
import Graphics.Vty.Input.Events
import Path
import Path.IO
import Sparep.Card
import System.Directory
import System.Exit
import Text.Show.Pretty
import YamlParse.Applicative

sparep :: IO ()
sparep = do
  p <- resolveFile' "/home/syd/src/german/vocab/das-geld.yaml"
  initialState <- buildInitialState p
  endState <- defaultMain tuiApp initialState
  print endState

data State
  = State
      { stateCursor :: NonEmptyCursor CardDef
      }
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
      appAttrMap = const $ attrMap mempty []
    }

buildInitialState :: Path Abs File -> IO State
buildInitialState cardDefsPath = do
  mcd <- readConfigFile cardDefsPath
  case mcd of
    Nothing -> die $ "File does not exist: " <> fromAbsFile cardDefsPath
    Just CardDefs {..} -> case NE.nonEmpty cardDefsCards of
      Nothing -> die "No cards to study."
      Just ne -> do
        let stateCursor = makeNonEmptyCursor ne
        pure State {..}

drawTui :: State -> [Widget ResourceName]
drawTui State {..} =
  let CardDef {..} = nonEmptyCursorCurrent stateCursor
   in [vBox [txt cardDefFront, txt cardDefBack]]

handleTuiEvent :: State -> BrickEvent n e -> EventM n (Next State)
handleTuiEvent s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt s
        _ -> continue s
    _ -> continue s

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Sparep.TUI.Draw where

import Brick.AttrMap
import Brick.Types
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Core
import Cursor.Brick
import Cursor.Simple.List.NonEmpty
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Path
import Sparep.Data
import Sparep.TUI.Repetition
import Sparep.TUI.State

drawTui :: State -> [Widget n]
drawTui =
  \case
    StateMenu ms -> drawMenuState ms
    StateDecks ds -> drawDecksState ds
    StateCards ds -> drawCardsState ds
    StateStudy ss -> drawStudyState ss

drawMenuState :: MenuState -> [Widget n]
drawMenuState MenuState {..} =
  [ vBox
      [ centerLayer $ withAttr headingAttr (str "SPAREP"),
        centerLayer
          $ border
          $ padAll 1
          $ vBox
          $ concat
            [ case menuStateSelection of
                Loading -> []
                Loaded Selection {..} ->
                  [ str $
                      unwords
                        [ "Done:",
                          show (length selectionTooSoon),
                          " Ready:",
                          show (length selectionReady),
                          " New:",
                          show (length selectionNew)
                        ]
                  ],
              [ padTop (Pad 1) $ str "Press enter to study now",
                str "Press d to show decks"
              ]
            ]
      ]
  ]

drawDecksState :: DecksState -> [Widget n]
drawDecksState DecksState {..} =
  [ vBox
      [ case decksStateCursor of
          Nothing -> str "No decks"
          Just cursor ->
            padBottom Max $
              hBox
                [ padAll 1 $ drawDeckList cursor,
                  vBorder,
                  padAll 1 $ uncurry drawDeckDetails (nonEmptyCursorCurrent cursor)
                ],
        hBorder,
        hCenterLayer $
          vBox
            [ str "Press enter to study the selected deck",
              str "Press c to show the cards in the selected deck"
            ]
      ]
  ]

drawDeckList :: NonEmptyCursor (RootedDeck, Loading (Selection Card)) -> Widget n
drawDeckList =
  verticalNonEmptyCursorTableWithHeader
    go
    (map (withAttr selectedAttr) . go)
    go
    ( map
        (withAttr headingAttr)
        [ str "Name",
          str "Done",
          str "Ready",
          str "New"
        ]
    )
  where
    go (RootedDeck _ Deck {..}, ls) =
      concat
        [ [ padRight Max $ txt $ fromMaybe " " deckName
          ],
          case ls of
            Loading ->
              [ str "Loading",
                str "Loading",
                str "Loading"
              ]
            Loaded Selection {..} ->
              [ str (show (length selectionTooSoon)),
                str (show (length selectionReady)),
                str (show (length selectionNew))
              ]
        ]

drawDeckDetails :: RootedDeck -> Loading (Selection Card) -> Widget n
drawDeckDetails (RootedDeck _ Deck {..}) ls =
  vBox $
    concat
      [ [ maybe emptyWidget (txtWrap . ("Name: " <>)) deckName,
          maybe emptyWidget (txtWrap . ("Description: " <>)) deckDescription,
          str " "
        ],
        case ls of
          Loading ->
            [ str "Loading",
              str "Loading",
              str "Loading",
              str "Loading"
            ]
          Loaded Selection {..} ->
            [ str ("Total:  " <> show (length selectionTooSoon + length selectionReady + length selectionNew)),
              str ("Done:  " <> show (length selectionTooSoon)),
              str ("Ready: " <> show (length selectionReady)),
              str ("New:   " <> show (length selectionNew))
            ]
      ]

drawCardsState :: CardsState -> [Widget n]
drawCardsState CardsState {..} =
  [ vBox
      [ padBottom Max $
          case cardsStateCursor of
            Nothing -> str "No cards"
            Just cursor ->
              hBox
                [ padAll 1 $ hLimit 16 $ drawCardList cursor,
                  vBorder,
                  padAll 1 $ uncurry drawCardDetails (nonEmptyCursorCurrent cursor)
                ],
        hBorder,
        hCenterLayer $
          vBox
            [ str "Press Enter to study this deck",
              str "Press Escape to exit"
            ]
      ]
  ]

drawCardList :: NonEmptyCursor (Card, Loading (Maybe (UTCTime, UTCTime))) -> Widget n
drawCardList =
  verticalNonEmptyCursorTableWithHeader
    go
    (map (withAttr selectedAttr) . go)
    go
    ( map
        (withAttr headingAttr)
        [ str "Id"
        ]
    )
  where
    go (c@Card {..}, _) =
      [ padRight Max $ txt $ renderCardIdHex $ hashCard c
      ]

drawCardDetails :: Card -> Loading (Maybe (UTCTime, UTCTime)) -> Widget n
drawCardDetails c@Card {..} lTimes =
  vBox $
    concat
      [ [txt $ "Id: " <> renderCardIdHex (hashCard c)],
        [withAttr instructionsAttr $ txtWrap $ "Instructions: " <> ins | ins <- maybeToList cardInstructions],
        [ hCenterLayer $ padAll 1 $ hLimit 40 $ border $
            vBox
              [ padAll 1 $ drawFrontSide cardFront,
                hBorder,
                padAll 1 $ drawBackSide cardBack
              ]
        ],
        case lTimes of
          Loading -> [str "Loading", str "Loading"]
          Loaded mTimes -> case mTimes of
            Nothing -> [str " ", str " "]
            Just (prevTime, nextTime) ->
              [ str $ "Last studied: " <> showTime prevTime,
                str $ "Next study: " <> showTime nextTime
              ]
      ]
  where
    showTime = formatTime defaultTimeLocale "%F %R"

cardSideDescription :: CardSide -> Text
cardSideDescription = \case
  TextSide t -> t
  SoundSide fp _ -> T.pack $ fromRelFile $ filename fp

drawStudyState :: StudyState -> [Widget n]
drawStudyState StudyState {..} =
  [ case studyStateCursor of
      Loading -> centerLayer $ str "Loading cards to study..."
      Loaded mCursor ->
        case mCursor of
          Nothing -> centerLayer $ str "Done"
          Just cursor ->
            vBox
              [ hCenterLayer
                  $ str
                  $ show (length (nonEmptyCursorNext cursor)) ++ " cards left",
                vCenterLayer $
                  vBox
                    [ drawCardStudy studyStateFrontBack (nonEmptyCursorCurrent cursor),
                      hCenterLayer $ padLeftRight 3 $
                        case studyStateFrontBack of
                          Front -> str "Show back: space"
                          Back -> padAll 1 $ str "Incorrect: i,  Hard: h,  Good: g,  Easy: e"
                    ]
              ]
  ]

drawCardStudy :: FrontBack -> Card -> Widget n
drawCardStudy fb Card {..} =
  vBox $
    concat
      [ [hCenterLayer $ padLeftRight 3 $ withAttr instructionsAttr $ txt ins | ins <- maybeToList cardInstructions],
        [ hCenterLayer $ padAll 1 $ hLimit 40
            $ border
            $ vBox
            $ concat
              [ [ padAll 1 $ drawFrontSide cardFront
                ],
                case fb of
                  Front -> []
                  Back ->
                    [ hBorder,
                      padAll 1 $ drawBackSide cardBack
                    ]
              ]
        ]
      ]

drawFrontSide :: CardSide -> Widget n
drawFrontSide = withAttr sideAttr . \case
  TextSide t -> txtWrap t
  SoundSide _ _ -> str "Press 'f' to play sound"

drawBackSide :: CardSide -> Widget n
drawBackSide = withAttr sideAttr . \case
  TextSide t -> txtWrap t
  SoundSide _ _ -> str "Press 'b' to play sound"

headingAttr :: AttrName
headingAttr = "heading"

selectedAttr :: AttrName
selectedAttr = "selected"

sideAttr :: AttrName
sideAttr = "side"

instructionsAttr :: AttrName
instructionsAttr = "instructions"

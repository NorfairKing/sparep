{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Sparep.TUI.Draw where

import Brick.AttrMap
import Brick.Types
import Brick.Util
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Core
import Cursor.Brick
import qualified Cursor.List.NonEmpty as NEC
import Cursor.Simple.List.NonEmpty
import Cursor.Text
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Graphics.Vty.Attributes
import Sparep.Data
import Sparep.TUI.State

drawTui :: State -> [Widget ResourceName]
drawTui =
  \case
    StateMenu ms -> drawMenuState ms
    StateDecks ds -> drawDecksState ds
    StateStudyUnits ds -> drawStudyUnitsState ds
    StateStudy ss -> drawStudyState ss

drawMenuState :: MenuState -> [Widget n]
drawMenuState MenuState {..} =
  [ vBox
      [ centerLayer $ withAttr headingAttr (str "SPAREP"),
        centerLayer $
          border $
            padAll 1 $
              vBox $
                concat
                  [ case menuStateSelection of
                      Loading -> []
                      Loaded Selection {..} ->
                        let numStr attr n =
                              (if n == 0 then id else withAttr attr) (str (show n))
                         in [ hBox
                                [ str "Done: ",
                                  numStr doneAttr $ length selectionTooSoon,
                                  str "  Ready: ",
                                  numStr readyAttr $ length selectionReady,
                                  str "  New: ",
                                  numStr newAttr $ length selectionNew
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
  [ joinBorders $
      vBox
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

drawDeckList :: NonEmptyCursor (RootedDeck, Loading (Selection a)) -> Widget n
drawDeckList =
  verticalNonEmptyCursorTableWithHeader
    go
    (map (forceAttr selectedAttr) . go)
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
    numStr attr n =
      (if n == 0 then id else withAttr attr) (str (show n))
    go (RootedDeck _ Deck {..}, ls) =
      concat
        [ [ padRight Max $ txt $ maybe " " unDeckName deckName
          ],
          case ls of
            Loading ->
              [ str "Loading",
                str "Loading",
                str "Loading"
              ]
            Loaded Selection {..} ->
              [ numStr doneAttr $ length selectionTooSoon,
                numStr readyAttr $ length selectionReady,
                numStr newAttr $ length selectionNew
              ]
        ]

drawDeckDetails :: RootedDeck -> Loading (Selection a) -> Widget n
drawDeckDetails (RootedDeck _ Deck {..}) ls =
  let numStr attr n =
        (if n == 0 then id else withAttr attr) (str (show n))
   in vBox $
        concat
          [ [ maybe emptyWidget (txtWrap . ("Name: " <>) . unDeckName) deckName,
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
                [ hBox
                    [ str "Total:  ",
                      numStr totalAttr (length selectionTooSoon + length selectionReady + length selectionNew)
                    ],
                  hBox
                    [ str "Done:  ",
                      numStr doneAttr (length selectionTooSoon)
                    ],
                  hBox
                    [ str "Ready: ",
                      numStr readyAttr (length selectionReady)
                    ],
                  hBox
                    [ str "New:   ",
                      numStr newAttr (length selectionNew)
                    ]
                ]
          ]

drawStudyUnitsState :: StudyUnitsState -> [Widget n]
drawStudyUnitsState StudyUnitsState {..} =
  [ joinBorders $
      vBox
        [ padBottom Max $
            case studyUnitsStateCursor of
              Nothing -> str "No cards"
              Just cursor ->
                hBox
                  [ padAll 1 $ hLimit 16 $ drawStudyUnitList cursor,
                    vBorder,
                    padAll 1 $ uncurry drawStudyUnitDetails (nonEmptyCursorCurrent cursor)
                  ],
          hBorder,
          hCenterLayer $
            vBox
              [ str "Press Enter to study this deck",
                str "Press Escape to exit"
              ]
        ]
  ]

drawStudyUnitList :: NonEmptyCursor (DefinitionContext StudyUnit, Loading (Maybe (UTCTime, UTCTime))) -> Widget n
drawStudyUnitList =
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
    go (DefinitionContext {..}, _) =
      [ padRight Max $ txt $ renderStudyUnitIdHex $ hashStudyUnit definitionContextUnit
      ]

drawStudyUnitDetails :: DefinitionContext StudyUnit -> Loading (Maybe (UTCTime, UTCTime)) -> Widget n
drawStudyUnitDetails DefinitionContext {..} lTimes =
  vBox $
    concat
      [ [txt $ "Id: " <> renderStudyUnitIdHex (hashStudyUnit definitionContextUnit)],
        [ withAttr deckNameAttr $ txt $ "Deck name: " <> dn
          | DeckName dn <- maybeToList definitionContextDeckName
        ],
        [ withAttr instructionsAttr $ txtWrap $ "Instructions: " <> ins
          | Instructions ins <- maybeToList definitionContextInstructions
        ],
        case definitionContextUnit of
          CardUnit card ->
            [ hCenterLayer $ drawCard Back card
            ]
          FillExerciseUnit FillExercise {..} ->
            [ hCenterLayer $
                border $
                  padAll 1 $
                    hBox $
                      map
                        ( \case
                            LitPart t -> withAttr litPartAttr $ txt t
                            FillPart t -> withAttr fillPartAttr $ txt t
                        )
                        (NE.toList fillExerciseSequence)
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

drawStudyState :: StudyState -> [Widget ResourceName]
drawStudyState StudyState {..} =
  [ case studyStateCursor of
      Loading -> centerLayer $ str "Loading cards to study..."
      Loaded mCursor ->
        case mCursor of
          Nothing -> centerLayer $ str "Done"
          Just cursor ->
            let StudyContext {..} = nonEmptyCursorCurrent cursor
             in vBox
                  [ hCenterLayer $
                      str $
                        unwords
                          [ show (length (nonEmptyCursorPrev cursor)),
                            "cards studied ",
                            show (length (nonEmptyCursorNext cursor)),
                            "cards left"
                          ],
                    vCenterLayer $
                      vBox $
                        concat
                          [ [hCenterLayer $ withAttr newLabelAttr (str "! New ! ") | studyContextNew],
                            [padAll 1 $ drawStudyUnitCursor studyContextUnit]
                          ]
                  ]
  ]

drawStudyUnitCursor :: DefinitionContext StudyUnitCursor -> Widget ResourceName
drawStudyUnitCursor du = case definitionContextUnit du of
  CardUnitCursor cc -> drawCardCursor $ cc <$ du
  FillExerciseUnitCursor fec -> drawFillExerciseCursor $ fec <$ du

drawCardCursor :: DefinitionContext CardCursor -> Widget n
drawCardCursor DefinitionContext {..} =
  let CardCursor {..} = definitionContextUnit
   in vBox $
        concat
          [ [ hCenterLayer $ withAttr deckNameAttr $ txt dn
              | DeckName dn <- maybeToList definitionContextDeckName
            ],
            [ padTop (Pad 1) $ hCenterLayer $ withAttr instructionsAttr $ txt ins
              | Instructions ins <- maybeToList definitionContextInstructions
            ],
            [ hCenterLayer $ drawCard cardCursorFrontBack cardCursorCard,
              vBox
                [ hCenterLayer $
                    case cardCursorFrontBack of
                      Front -> str "Show back: space,  Undo: u"
                      Back -> padAll 1 $ str "Incorrect: i,  Hard: h,  Good: g,  Easy: e"
                ]
            ]
          ]

drawCard :: FrontBack -> Card -> Widget n
drawCard fb Card {..} =
  padAll 1 $
    hLimit 40 $
      joinBorders $
        border $
          vBox $
            concat
              [ [ padAll 1 $ drawFrontSide cardFront
                ],
                case fb of
                  Front -> []
                  Back ->
                    [ hBorder,
                      padAll 1 $ drawBackSide cardBack
                    ]
              ]

drawFrontSide :: CardSide -> Widget n
drawFrontSide =
  withAttr sideAttr . \case
    TextSide t -> txtWrap t
    SoundSide _ _ -> str "Press 'f' to play sound"
    ImageSide _ _ -> str "Press 'f' to show image"

drawBackSide :: CardSide -> Widget n
drawBackSide =
  withAttr sideAttr . \case
    TextSide t -> txtWrap t
    SoundSide _ _ -> str "Press 'b' to play sound"
    ImageSide _ _ -> str "Press 'b' to show image"

drawFillExerciseCursor :: DefinitionContext FillExerciseCursor -> Widget ResourceName
drawFillExerciseCursor DefinitionContext {..} =
  let fec@FillExerciseCursor {..} = definitionContextUnit
      partCursorMarkup =
        \case
          LitPartCursor t -> withAttr litPartAttr $ txt t
          FillPartCursor tc t ->
            let t' = rebuildTextCursor tc
                attr =
                  if t' == t
                    then fillCorrectAttr
                    else fillIncorrectAttr
             in withAttr attr $ txt (if T.null t' then "___" else t')
   in vBox $
        concat
          [ [ hCenterLayer $ withAttr deckNameAttr $ txt dn
              | DeckName dn <- maybeToList definitionContextDeckName
            ],
            [ padTop (Pad 1) $ hCenterLayer $ withAttr instructionsAttr $ txt ins
              | Instructions ins <- maybeToList definitionContextInstructions
            ],
            [ hCenterLayer $
                border . padAll 1 $
                  NEC.foldNonEmptyCursor
                    ( \befores current afters ->
                        hBox $
                          concat
                            [ map partCursorMarkup befores,
                              case current of
                                LitPartCursor t -> [withAttr litPartAttr $ txt t] -- Should not happen.
                                FillPartCursor tc t ->
                                  let t' = rebuildTextCursor tc
                                      attr =
                                        if t' == t
                                          then fillCorrectAttr
                                          else fillPartAttr
                                      (t1, t2) = textCursorSplit tc
                                   in if fillExerciseCursorShow
                                        then
                                          [ if t' == t
                                              then withAttr fillCorrectAttr $ txt t'
                                              else withAttr fillShownAttr $ txt t
                                          ]
                                        else case T.unpack (rebuildTextCursor t2) of
                                          [] ->
                                            [ withAttr attr $ txt $ rebuildTextCursor t1,
                                              withAttr (attr <> fillCursorPartAttr) $ txt $ T.singleton ' '
                                            ]
                                          (c : _) ->
                                            [ withAttr attr $ txt $ rebuildTextCursor t1,
                                              withAttr (attr <> fillCursorPartAttr) $ txt $ T.singleton c,
                                              withAttr attr $ txt $ rebuildTextCursor t2
                                            ],
                              map partCursorMarkup afters
                            ]
                    )
                    fillExerciseCursorList
            ],
            [hCenterLayer $ padAll 1 $ str "Next hole: Tab,  Previous hole: Shift-Tab" | fillExerciseCursorCountHoles fec > 1],
            [ hCenterLayer $
                padAll 1 $
                  if fillExerciseCursorCorrect fec
                    then str "Incorrect: Alt-i,  Hard: Alt-h,  Good: Alt-g,  Easy: Alt-e"
                    else str "Incorrect: Alt-i,  Show solution: Alt-<space>,  Undo: Alt-u"
            ]
          ]

tuiAttrMap :: AttrMap
tuiAttrMap =
  attrMap
    defAttr
    [ (selectedAttr, fg brightWhite),
      (headingAttr, defAttr `withStyle` underline),
      (deckNameAttr, fg yellow `withStyle` underline),
      (instructionsAttr, fg yellow),
      (sideAttr, fg brightWhite),
      (litPartAttr, fg brightWhite),
      (fillPartAttr, fg magenta),
      (fillIncorrectAttr, fg red),
      (fillCorrectAttr, fg green),
      (fillPartAttr <> fillCursorPartAttr, withStyle (fg magenta) reverseVideo),
      (fillCorrectAttr <> fillCursorPartAttr, withStyle (fg green) reverseVideo),
      (fillShownAttr, fg blue),
      (totalAttr, fg blue),
      (doneAttr, fg green),
      (readyAttr, fg yellow),
      (newAttr, fg red),
      (newLabelAttr, fg green)
    ]

headingAttr :: AttrName
headingAttr = "heading"

selectedAttr :: AttrName
selectedAttr = "selected"

sideAttr :: AttrName
sideAttr = "side"

litPartAttr :: AttrName
litPartAttr = "lit-part"

fillPartAttr :: AttrName
fillPartAttr = "fill-part"

fillCursorPartAttr :: AttrName
fillCursorPartAttr = "fill-part"

fillCorrectAttr :: AttrName
fillCorrectAttr = "fill-part-incorrect"

fillIncorrectAttr :: AttrName
fillIncorrectAttr = "fill-part-correct"

fillShownAttr :: AttrName
fillShownAttr = "fill-shown"

deckNameAttr :: AttrName
deckNameAttr = "deck-name"

instructionsAttr :: AttrName
instructionsAttr = "instructions"

totalAttr :: AttrName
totalAttr = "total"

doneAttr :: AttrName
doneAttr = "done"

readyAttr :: AttrName
readyAttr = "ready"

newAttr :: AttrName
newAttr = "new"

newLabelAttr :: AttrName
newLabelAttr = "new-label"

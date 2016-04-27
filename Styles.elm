module Styles (..) where

import NativeUi.Style exposing (..)
import NativeUi exposing (style, Property)


type alias Colors =
  { offBlack : String
  , offWhite : String
  , green : String
  , red : String
  , lightGrey : String
  , midGrey : String
  }


colors : Colors
colors =
  { offBlack = "#333"
  , offWhite = "#F5F5F5"
  , green = "#5CEC3D"
  , red = "#EC483D"
  , lightGrey = "#CCC"
  , midGrey = "#777"
  }


baseFontSize : Float
baseFontSize =
  16


spacer : Float
spacer =
  16


buttonBackground : String -> String
buttonBackground actionName =
  case actionName of
    "Start" ->
      colors.green

    "Stop" ->
      colors.red

    "Lap" ->
      colors.lightGrey

    "Reset" ->
      colors.lightGrey

    _ ->
      ""


actionButton : String -> Property
actionButton actionName =
  style
    [ flex 1
    , height (2 * spacer)
    , alignItems "center"
    , justifyContent "center"
    , paddingLeft spacer
    , paddingRight spacer
    , backgroundColor (buttonBackground actionName)
    ]


buttonRow : Property
buttonRow =
  style
    [ flex 1
    , flexDirection "row"
    ]


mainView : Property
mainView =
  style
    [ flex 1 ]


header : Property
header =
  style
    [ borderBottomColor colors.offBlack
    , borderBottomWidth 1
    , paddingTop (1.5 * spacer)
    , paddingBottom (0.5 * spacer)
    , paddingLeft spacer
    , paddingRight spacer
    , color "#f5f5f5"
    , letterSpacing (0.4 * baseFontSize)
    , textAlign "center"
    ]


laps : Property
laps =
  style
    [ padding 0
    , textAlign "center"
    , backgroundColor colors.offWhite
    ]


lapTimer : Property
lapTimer =
  style
    [ textAlign "right"
    , color colors.midGrey
    ]


lapEntry : Property
lapEntry =
  style
    [ paddingTop (0.5 * spacer)
    , paddingBottom (0.5 * spacer)
    , borderBottomWidth 1
    , borderBottomColor colors.lightGrey
    , flex 1
    , flexDirection "row"
    ]


lapNumber : Property
lapNumber =
  style
    [ width (2 * spacer)
    , color colors.midGrey
    ]


lapTime : Property
lapTime =
  style
    [ flex 1
    , paddingRight (2 * spacer)
    ]


textBlock : Property
textBlock =
  style
    [ margin (1 * baseFontSize)
    , textAlign "center"
    ]


timers : Property
timers =
  style
    [ paddingTop spacer
    , paddingBottom spacer
    ]


totalTimer : Property
totalTimer =
  style
    []


timersWrapper : Property
timersWrapper =
  style [ textAlign "center" ]

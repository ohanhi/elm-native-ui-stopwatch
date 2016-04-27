module Main (..) where

import Signal
import String
import Time exposing (Time)
import NativeUi as Ui
import NativeUi.Elements as El exposing (..)
import NativeUi.Handlers exposing (..)
import Styles


--
-- app : Signal Ui.NativeUi
-- app =
--   NativeApp.start { model = model, view = view, update = update }
--


type Action
  = Start
  | Stop
  | Lap
  | Reset
  | NoOp


actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp


millis : Signal Time
millis =
  Time.every (Time.millisecond * 16)


viewLine : String -> Ui.NativeUi
viewLine lineText =
  El.view
    [ Styles.textBlock ]
    [ Ui.string lineText ]


millisToString : Time -> String
millisToString millis =
  let
    minutes =
      floor (millis / (60 * 1000))

    seconds =
      floor (millis / 1000) `rem` 60

    centiseconds =
      (floor millis `rem` 1000) // 10

    pad n =
      String.padLeft 2 '0' (toString n)
  in
    (pad minutes) ++ ":" ++ (pad seconds) ++ "," ++ (pad centiseconds)


buttonRow : Bool -> Ui.NativeUi
buttonRow isTiming =
  El.view
    [ Styles.buttonRow ]
    <| if isTiming then
        [ actionButton Stop
        , actionButton Lap
        ]
       else
        [ actionButton Start
        , actionButton Reset
        ]


actionButton : Action -> Ui.NativeUi
actionButton action =
  text
    [ onPress actions.address action
    , Styles.actionButton (toString action)
    ]
    [ Ui.string (toString action) ]


lapsList : List Time -> Ui.NativeUi
lapsList laps =
  view
    [ Styles.laps ]
    (laps
      |> List.reverse
      |> List.indexedMap lapEntry
      |> List.reverse
    )


lapEntry : Int -> Time -> Ui.NativeUi
lapEntry index entry =
  let
    lapNumber =
      (toString (index + 1)) ++ "."
  in
    view
      [ Styles.lapEntry ]
      [ text [ Styles.lapNumber ] [ Ui.string lapNumber ]
      , text [ Styles.lapTime ] [ Ui.string (millisToString entry) ]
      ]


timers : Model -> Ui.NativeUi
timers model =
  let
    timeDiff =
      model.currentTimestamp - model.startTimestamp

    currentDuration =
      if model.isTiming then
        model.durationOnStop + timeDiff
      else
        model.durationOnStop

    lapDuration =
      currentDuration - total model.laps
  in
    El.view
      [ Styles.timersWrapper ]
      [ El.view
          [ Styles.timers ]
          [ El.view
              [ Styles.lapTimer ]
              [ El.text [] [ Ui.string (millisToString lapDuration) ] ]
          , El.view
              [ Styles.totalTimer ]
              [ El.text [] [ Ui.string (millisToString currentDuration) ] ]
          ]
      ]


header : String -> Ui.NativeUi
header heading =
  El.view
    [ Styles.header ]
    [ El.text [] [ Ui.string heading ] ]


mainView : String -> Model -> Ui.NativeUi
mainView heading model =
  El.view
    [ Styles.mainView ]
    [ header heading
    , timers model
    , buttonRow model.isTiming
    , lapsList model.laps
    ]


type alias Model =
  { startTimestamp : Time
  , laps : List Time
  , isTiming : Bool
  , currentTimestamp : Time
  , durationOnStop : Time
  , lapDurationOnStop : Time
  , lastLapTimestamp : Time
  }


initial : Model
initial =
  { startTimestamp = 0
  , laps = []
  , isTiming = False
  , currentTimestamp = 0
  , durationOnStop = 0
  , lapDurationOnStop = 0
  , lastLapTimestamp = 0
  }


handleStart : Time -> Model -> Model
handleStart time state =
  { state
    | isTiming = True
    , startTimestamp = time
    , lastLapTimestamp = time
  }


handleStop : Time -> Model -> Model
handleStop time state =
  let
    duration =
      state.durationOnStop + time - state.startTimestamp
  in
    { state
      | isTiming = False
      , durationOnStop = duration
      , lapDurationOnStop = duration - (total state.laps)
    }


handleLap : Time -> Model -> Model
handleLap time state =
  { state
    | laps =
        (state.lapDurationOnStop + time - state.lastLapTimestamp)
          :: state.laps
    , lastLapTimestamp = time
    , lapDurationOnStop = 0
  }


update : StateChange -> Model -> Model
update change state =
  case change of
    ButtonPress ( time, action ) ->
      case action of
        Start ->
          handleStart time state

        Stop ->
          handleStop time state

        Lap ->
          handleLap time state

        Reset ->
          initial

        NoOp ->
          state

    TimeChange time ->
      if state.isTiming then
        { state | currentTimestamp = time }
      else
        state


total : List Time -> Time
total laps =
  List.foldl (+) 0 laps


type StateChange
  = TimeChange Time
  | ButtonPress ( Time, Action )


inputSignal : Signal StateChange
inputSignal =
  Signal.mergeMany
    [ Signal.map TimeChange millis
    , Signal.map ButtonPress (Time.timestamp actions.signal) |> (Signal.map (Debug.log "input"))
    ]


model : Signal Model
model =
  Signal.foldp update initial inputSignal


main : Signal Ui.NativeUi
main =
  Signal.map (mainView "Chronographify") model

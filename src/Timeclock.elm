module Timeclock exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Task exposing (Task)
import Time exposing (..)



-- Add Time Zone
-- Ticking Duration


type alias Model =
    { currentTime : Time.Posix
    , startTime : Time.Posix
    , stopTime : Time.Posix
    , zone : Time.Zone
    }


initialModel : Model
initialModel =
    { currentTime = Time.millisToPosix 0
    , startTime = Time.millisToPosix 0
    , stopTime = Time.millisToPosix 0
    , zone = Time.utc
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Task.perform AdjustTimeZone Time.here
    )


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, Task.perform AdjustTimeZone Time.here )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Time Clock" ]
        , p [] [ text ("Current Time: " ++ viewHumanTime model.zone model.currentTime) ]
        , button [ onClick ClockInClick ] [ text "Clock In" ]
        , button [ onClick ClockOutClick ] [ text "Clock Out" ]
        , button [ onClick Clear ] [ text "Clear" ]
        , hr [] []
        , p [] [ text ("Start: " ++ viewHumanTime model.zone model.startTime) ]
        , p [] [ text ("Stop: " ++ viewHumanTime model.zone model.stopTime) ]
        , viewTimeWorked model.zone model.startTime model.stopTime
        ]


humanTime : Time.Zone -> Posix -> ( String, String, String )
humanTime zone time =
    let
        hour =
            String.fromInt (Time.toHour zone time)

        minute =
            String.fromInt (Time.toMinute zone time)

        second =
            String.fromInt (Time.toSecond zone time)
    in
    ( hour, minute, second )


viewHumanTime : Time.Zone -> Posix -> String
viewHumanTime zone time =
    let
        ( hour, minute, second ) =
            humanTime zone time
    in
    hour ++ ":" ++ minute ++ ":" ++ second


localHumanTime : Time.Posix -> String
localHumanTime time =
    viewHumanTime utc time


viewTimeWorked : Time.Zone -> Time.Posix -> Time.Posix -> Html Msg
viewTimeWorked zone startTime stopTime =
    let
        ( hour, minute, second ) =
            humanTime utc (Time.millisToPosix (Time.posixToMillis stopTime - Time.posixToMillis startTime))
    in
    h2 []
        [ text "Total Time Worked"
        , h3 [] [ text ("Hours " ++ hour) ]
        , h3 [] [ text ("Minutes " ++ minute) ]
        , h3 [] [ text ("Seconds " ++ second) ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick


type Msg
    = AdjustTimeZone Time.Zone
    | Clear
    | ClockIn Time.Posix
    | ClockOut Time.Posix
    | ClockInClick
    | ClockOutClick
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AdjustTimeZone newZone ->
            ( { model | zone = newZone }, Cmd.none )

        Clear ->
            ( { model | startTime = Time.millisToPosix 0, stopTime = Time.millisToPosix 0 }, Cmd.none )

        ClockInClick ->
            ( model, Task.perform ClockIn Time.now )

        ClockOutClick ->
            ( model, Task.perform ClockOut Time.now )

        ClockIn time ->
            ( { model | startTime = time }, Cmd.none )

        ClockOut time ->
            ( { model | stopTime = time }, Cmd.none )

        Tick time ->
            ( { model | currentTime = time }, Cmd.none )

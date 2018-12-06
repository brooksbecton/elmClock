module Timeclock exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Task exposing (Task)
import Time exposing (..)

-- Add Time Zone
-- Ticking Clock
-- Ticking Duration

type alias Model =
    { startTime : Time.Posix
    , stopTime : Time.Posix
    }


initialModel =
    { startTime = Time.millisToPosix 0
    , stopTime = Time.millisToPosix 0
    }


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Time Clock" ]
        , button [ onClick ClockInClick ] [ text "Clock In" ]
        , button [ onClick ClockOutClick ] [ text "Clock Out" ]
        , button [ onClick Clear ] [ text "Clear" ]
        , hr [] []
        , p [] [ text ("Start: " ++ humanTime model.startTime) ]
        , p [] [ text ("Stop: " ++ humanTime model.stopTime) ]
        , p [] [ text ("Total Time Worked: " ++ viewTimeWorked model.startTime model.stopTime) ]
        ]


humanTime : Posix -> String
humanTime time =
    let
        hour =
            String.fromInt (Time.toHour utc time)

        minute =
            String.fromInt (Time.toMinute utc time)

        second =
            String.fromInt (Time.toSecond utc time)
    in
    hour ++ ":" ++ minute ++ ":" ++ second


viewTimeWorked : Time.Posix -> Time.Posix -> String
viewTimeWorked startTime stopTime =
    humanTime (Time.millisToPosix (Time.posixToMillis stopTime - Time.posixToMillis startTime))


type Msg
    = Clear
    | ClockIn Time.Posix
    | ClockOut Time.Posix
    | ClockInClick
    | ClockOutClick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

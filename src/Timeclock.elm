module Timeclock exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Task exposing (Task)
import Time exposing (..)


type alias Model =
    { currentTime : Time.Posix
    , startTime : Time.Posix
    , stopTime : Time.Posix
    , zone : Time.Zone
    }


epoch : Time.Posix
epoch =
    Time.millisToPosix 0


initialModel : Model
initialModel =
    { currentTime = epoch
    , startTime = epoch
    , stopTime = epoch
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
    let
        viewLocalTime =
            viewHumanTime model.zone
    in
    div []
        [ div [ class "container" ]
            [ div
                [ class "row" ]
                [ div [ class "col" ] []
                , div [ class "col-10" ]
                    [ div [ class "Header" ]
                        [ h1
                            []
                            [ text "Time Clock" ]
                        , p []
                            [ text ("Current Time: " ++ viewLocalTime model.currentTime) ]
                        ]
                    , div [ class "PunchControl" ]
                        [ div [ class "sticky" ]
                            [ p []
                                [ span [] [ text "Start: " ]
                                , span
                                    [ classList [ ( "bounceIn", model.startTime /= epoch ) ] ]
                                    [ text
                                        (if model.startTime /= epoch then
                                            viewLocalTime model.startTime

                                         else
                                            ""
                                        )
                                    ]
                                ]
                            , p []
                                [ span [] [ text "Stop: " ]
                                , span [ classList [ ( "bounceIn", model.stopTime /= epoch ) ] ]
                                    [ text
                                        (if model.stopTime /= epoch then
                                            viewLocalTime model.stopTime

                                         else
                                            ""
                                        )
                                    ]
                                ]
                            , viewAccruedTime model.startTime model.stopTime model.currentTime
                            ]
                        ]
                    ]
                , div [ class "col" ] []
                ]
            ]
        , footer
            [ class "Footer" ]
            [ button [ class "btn", onClick ClockInClick, disabled (model.startTime /= epoch) ] [ text "Clock In" ]
            , button [ class "btn", onClick ClockOutClick, disabled (model.stopTime /= epoch) ] [ text "Clock Out" ]
            , button [ class "btn", onClick Clear ] [ text "Clear" ]
            ]
        ]


humanTime : Time.Zone -> Posix -> ( Int, Int, Int )
humanTime zone time =
    let
        hour =
            Time.toHour zone time

        minute =
            Time.toMinute zone time

        second =
            Time.toSecond zone time
    in
    ( hour, minute, second )


viewHumanTime : Time.Zone -> Posix -> String
viewHumanTime zone time =
    let
        ( hour, minute, second ) =
            humanTime zone time
    in
    String.fromInt (remainderBy 12 hour) ++ ":" ++ String.fromInt minute ++ ":" ++ String.fromInt second


viewAccruedTime : Time.Posix -> Time.Posix -> Time.Posix -> Html Msg
viewAccruedTime startTime stopTime currentTime =
    let
        timeWorked =
            if stopTime == epoch then
                Time.millisToPosix (Time.posixToMillis currentTime - Time.posixToMillis startTime)

            else
                Time.millisToPosix (Time.posixToMillis stopTime - Time.posixToMillis startTime)

        hoursWorked =
            Time.toHour utc timeWorked

        minutesWorked =
            Time.toMinute utc timeWorked

        secondsWorked =
            Time.toSecond utc timeWorked
    in
    if startTime /= epoch && currentTime /= epoch then
        div []
            [ p [] [ text ("Hours Worked: " ++ String.fromInt hoursWorked) ]
            , p [] [ text ("Minutes Worked: " ++ String.fromInt minutesWorked) ]
            , p [] [ text ("Seconds Worked: " ++ String.fromInt secondsWorked) ]
            ]

    else
        div []
            [ p [] [ text "Hours Worked: 0" ]
            , p [] [ text "Minutes Worked: 0" ]
            , p [] [ text "Seconds Worked: 0" ]
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
            ( { model | startTime = epoch, stopTime = epoch }, Cmd.none )

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

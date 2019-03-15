module Main exposing (main)

import Browser exposing (Document)
import Clock
import DatePicker
import DatePicker.Types exposing (DateLimit(..), ViewType(..))
import DateTime exposing (DateTime)
import Html exposing (Html, br, button, div, text)
import Task
import Time
import TimePicker.Types as TimePicker


type alias Flags =
    ()


type alias Model =
    { datePicker : Maybe DatePicker.Model
    , selectedDateTime : Maybe DateTime
    }


type Msg
    = Initialise Time.Posix
    | DatePickerMsg DatePicker.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Initialise today ->
            let
                ( date1, date2 ) =
                    ( DateTime.fromRawParts
                        { day = 1, month = Time.Jan, year = 2019 }
                        { hours = 0, minutes = 0, seconds = 0, milliseconds = 0 }
                    , DateTime.fromRawParts
                        { day = 31, month = Time.Dec, year = 2019 }
                        { hours = 0, minutes = 0, seconds = 0, milliseconds = 0 }
                    )

                calendarConfig =
                    { today = DateTime.fromPosix today
                    , primaryDate = Nothing
                    , dateLimit =
                        case ( date1, date2 ) of
                            ( Just d1, Just d2 ) ->
                                DateLimit { minDate = d1, maxDate = d2 }

                            _ ->
                                NoLimit { disablePastDates = False }
                    }

                timePickerConfig =
                    Just
                        { pickerType = TimePicker.HH_MM { hoursStep = 1, minutesStep = 5 }
                        , defaultTime = Clock.midnight
                        , pickerTitle = "Date Time"
                        }
            in
            ( { model
                | datePicker =
                    Just (DatePicker.initialise Single calendarConfig timePickerConfig)
              }
            , Cmd.none
            )

        DatePickerMsg subMsg ->
            case model.datePicker of
                Just datePicker ->
                    let
                        ( subModel, subCmd, extMsg ) =
                            DatePicker.update subMsg datePicker

                        selectedDateTime =
                            case extMsg of
                                DatePicker.DateSelected dateTime ->
                                    dateTime

                                DatePicker.None ->
                                    model.selectedDateTime
                    in
                    ( { model
                        | datePicker = Just subModel
                        , selectedDateTime = selectedDateTime
                      }
                    , Cmd.map DatePickerMsg subCmd
                    )

                Nothing ->
                    ( model
                    , Cmd.none
                    )


view : Model -> Document Msg
view model =
    { title = "DatePicker example"
    , body =
        [ div []
            [ case model.datePicker of
                Just datePicker ->
                    Html.map DatePickerMsg (DatePicker.view datePicker)

                Nothing ->
                    text ""
            ]
        , br [] []
        , br [] []
        , br [] []
        , text "Selected DateTime"
        , br [] []
        , text (toHumanReadableString model.selectedDateTime)
        ]
    }


toHumanReadableString : Maybe DateTime -> String
toHumanReadableString dateTime =
    case dateTime of
        Just dt ->
            String.join " "
                [ String.join " "
                    [ weekdayToString (DateTime.getWeekday dt)
                    , String.fromInt (DateTime.getDay dt)
                    , monthToString (DateTime.getMonth dt)
                    , String.fromInt (DateTime.getYear dt)
                    ]
                , String.join ":"
                    [ formatTime (DateTime.getHours dt)
                    , formatTime (DateTime.getMinutes dt)
                    , formatTime (DateTime.getSeconds dt)
                    , formatMillis (DateTime.getMilliseconds dt)
                    ]
                ]

        Nothing ->
            "Nothing"


formatTime : Int -> String
formatTime time =
    if time < 10 then
        "0" ++ String.fromInt time

    else
        String.fromInt time


formatMillis : Int -> String
formatMillis millis =
    if millis < 10 then
        "00" ++ String.fromInt millis

    else if millis < 100 then
        "0" ++ String.fromInt millis

    else
        String.fromInt millis


weekdayToString : Time.Weekday -> String
weekdayToString weekday =
    case weekday of
        Time.Mon ->
            "Monday"

        Time.Tue ->
            "Tuesday"

        Time.Wed ->
            "Wednesday"

        Time.Thu ->
            "Thursday"

        Time.Fri ->
            "Friday"

        Time.Sat ->
            "Saturday"

        Time.Sun ->
            "Sunday"


monthToString : Time.Month -> String
monthToString month =
    case month of
        Time.Jan ->
            "January"

        Time.Feb ->
            "February"

        Time.Mar ->
            "March"

        Time.Apr ->
            "April"

        Time.May ->
            "May"

        Time.Jun ->
            "June"

        Time.Jul ->
            "July"

        Time.Aug ->
            "August"

        Time.Sep ->
            "September"

        Time.Oct ->
            "October"

        Time.Nov ->
            "November"

        Time.Dec ->
            "December"


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { datePicker = Nothing
      , selectedDateTime = Nothing
      }
    , Task.perform Initialise Time.now
    )


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

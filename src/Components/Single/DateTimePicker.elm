module Components.Single.DateTimePicker exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Clock
import DatePicker
import DatePicker.Types exposing (DateLimit(..), ViewType(..))
import DateTime exposing (DateTime)
import Extra.DateTime as DateTimeExtra
import Html exposing (Html, br, div, h3, span, text)
import Html.Attributes exposing (class)
import Time exposing (Posix)
import TimePicker.Types as TimePicker


type alias Model =
    { datePicker : DatePicker.Model
    , selectedDateTime : Maybe DateTime
    }


init : Posix -> Model
init todayPosix =
    let
        today =
            DateTime.fromPosix todayPosix

        -- Allow 6 months in advance and in the past selection.
        ( minDate, maxDate ) =
            let
                sixMonthDays =
                    186
            in
            ( DateTimeExtra.decrementDays sixMonthDays today
            , DateTimeExtra.incrementDays sixMonthDays today
            )

        calendarConfig =
            { today = today
            , primaryDate = Nothing
            , dateLimit = DateLimit { minDate = minDate, maxDate = maxDate }
            }

        timePickerConfig =
            Just
                { pickerType = TimePicker.HH_MM { hoursStep = 1, minutesStep = 5 }
                , defaultTime = Clock.midnight
                , pickerTitle = "Select Time"
                }
    in
    { datePicker =
        DatePicker.initialise Single calendarConfig timePickerConfig
    , selectedDateTime = Nothing
    }


type Msg
    = NoOp
    | DatePickerMsg DatePicker.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )

        DatePickerMsg subMsg ->
            let
                ( updated, subCmd, extMsg ) =
                    DatePicker.update subMsg model.datePicker

                selectedDateTime =
                    case extMsg of
                        DatePicker.None ->
                            model.selectedDateTime

                        DatePicker.DateSelected dateTime ->
                            dateTime
            in
            ( { model
                | datePicker = updated
                , selectedDateTime = selectedDateTime
              }
            , Cmd.map DatePickerMsg subCmd
            )


view : Model -> Html Msg
view { datePicker, selectedDateTime } =
    div [ class "section" ]
        [ h3 [] [ text "Single Date-Time Picker" ]
        , Html.map DatePickerMsg (DatePicker.view datePicker)
        , br [] []
        , case selectedDateTime of
            Just sdt ->
                div [ class "footer" ]
                    [ span [ class "text" ] [ text "Selected DateTime: " ]
                    , span [ class "date" ] [ text (DateTimeExtra.toString sdt) ]
                    ]

            Nothing ->
                div [ class "footer" ]
                    [ text "No \"selected date\" yet"
                    ]
        ]

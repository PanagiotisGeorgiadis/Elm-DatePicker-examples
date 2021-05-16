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
import Extra.I18n exposing (Language, getI18n, timePickerI18n)
import Html exposing (Html, br, div, h3, span, text)
import Html.Attributes exposing (class)
import Time exposing (Posix)
import TimePicker.Types as TimePicker


type alias Model =
    { picker : DatePicker.Model
    , selectedDateTime : Maybe DateTime
    }


init : Language -> Posix -> Model
init language todayPosix =
    let
        today =
            DateTime.fromPosix todayPosix

        -- Allow 6 months in advance and in the past selection.
        ( minDate, maxDate ) =
            let
                ( past, future ) =
                    ( DateTimeExtra.decrementMonths 6 today
                    , DateTimeExtra.incrementMonths 6 today
                    )
            in
            ( Maybe.withDefault past <| DateTime.setDay 1 past
            , Maybe.withDefault future <| DateTime.setDay (DateTime.lastDayOf future) future
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
                , pickerTitle = timePickerI18n language
                }

        i18n =
            Just (getI18n language)
    in
    { picker =
        DatePicker.initialise Single calendarConfig timePickerConfig i18n
    , selectedDateTime = Nothing
    }


type Msg
    = PickerMsg DatePicker.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PickerMsg subMsg ->
            let
                ( updated, subCmd, extMsg ) =
                    DatePicker.update subMsg model.picker

                selectedDateTime =
                    case extMsg of
                        DatePicker.None ->
                            model.selectedDateTime

                        DatePicker.DateSelected dateTime ->
                            dateTime
            in
            ( { model
                | picker = updated
                , selectedDateTime = selectedDateTime
              }
            , Cmd.map PickerMsg subCmd
            )


view : Model -> Html Msg
view { picker, selectedDateTime } =
    div [ class "section" ]
        [ h3 [] [ text "Single Date-Time Picker" ]
        , Html.map PickerMsg (DatePicker.view picker)
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

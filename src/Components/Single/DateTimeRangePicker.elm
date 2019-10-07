module Components.Single.DateTimeRangePicker exposing (..)

import Clock
import DateRangePicker exposing (SelectedDateRange)
import DateRangePicker.Types exposing (DateLimit(..), ViewType(..))
import DateTime
import Extra.DateTime as DateTimeExtra
import Html exposing (Html, br, div, h3, span, text)
import Html.Attributes exposing (class)
import Time exposing (Posix)
import TimePicker.Types as TimePicker


type alias Model =
    { picker : DateRangePicker.Model
    , selectedRange : Maybe SelectedDateRange
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
            , dateRangeOffset = Just { minDateRangeLength = 7 }
            }

        -- The `timePickerConfig` is set to `Nothing` cause we don't want one.
        timePickerConfig =
            Just
                { pickerType = TimePicker.HH_MM { hoursStep = 1, minutesStep = 5 }
                , defaultTime = Clock.midnight
                , pickerTitles = { start = "Start Date Time", end = "End Date Time" }
                , mirrorTimes = True
                }
    in
    { picker = DateRangePicker.initialise Single calendarConfig timePickerConfig
    , selectedRange = Nothing
    }


type Msg
    = PickerMsg DateRangePicker.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PickerMsg subMsg ->
            let
                ( updated, subCmd, extMsg ) =
                    DateRangePicker.update subMsg model.picker

                selectedRange =
                    case extMsg of
                        DateRangePicker.None ->
                            model.selectedRange

                        DateRangePicker.DateRangeSelected range ->
                            range
            in
            ( { model
                | picker = updated
                , selectedRange = selectedRange
              }
            , Cmd.map PickerMsg subCmd
            )


view : Model -> Html Msg
view { picker, selectedRange } =
    div [ class "section" ]
        [ h3 [] [ text "Single Date-Time Range Picker" ]
        , Html.map PickerMsg (DateRangePicker.view picker)
        , br [] []
        , case selectedRange of
            Just { startDate, endDate } ->
                div [ class "footer" ]
                    [ span [ class "text" ] [ text "Selected Date Range: " ]
                    , span [ class "date" ] [ text (DateTimeExtra.toString startDate) ]
                    , span [ class "separator" ] [ text "—" ]
                    , span [ class "date" ] [ text (DateTimeExtra.toString endDate) ]
                    ]

            Nothing ->
                div [ class "footer" ]
                    [ text "No \"selected range\" yet"
                    ]
        ]

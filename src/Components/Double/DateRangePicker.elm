module Components.Double.DateRangePicker exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import DateRangePicker exposing (SelectedDateRange)
import DateRangePicker.Types exposing (DateLimit(..), ViewType(..))
import DateTime
import Extra.DateTime as DateTimeExtra
import Extra.I18n exposing (Language, getI18n)
import Html exposing (Html, br, div, h3, span, text)
import Html.Attributes exposing (class)
import Time exposing (Posix)


type alias Model =
    { picker : DateRangePicker.Model
    , selectedRange : Maybe SelectedDateRange
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
            , dateRangeOffset = Just { minDateRangeLength = 7 }
            }

        -- The `timePickerConfig` is set to `Nothing` cause we don't want one.
        timePickerConfig =
            Nothing

        i18n =
            Just (getI18n language)
    in
    { picker =
        DateRangePicker.initialise Double calendarConfig timePickerConfig i18n
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
        [ h3 [] [ text "Double Date Range Picker" ]
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

module Components.WithInput.Single.DateRangePicker exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Events as Browser
import DateRangePicker exposing (SelectedDateRange)
import DateRangePicker.Types exposing (DateLimit(..), ViewType(..))
import DateTime
import Extra.Date as DateExtra
import Extra.DateTime as DateTimeExtra
import Html exposing (Html, br, div, h3, input, span, text)
import Html.Attributes exposing (class, readonly, value)
import Html.Events exposing (onFocus, stopPropagationOn)
import Json.Decode as Decode
import Time exposing (Posix)


type alias Model =
    { picker : DateRangePicker.Model
    , selectedRange : Maybe SelectedDateRange
    , isFocused : Bool
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
            Nothing
    in
    { picker = DateRangePicker.initialise Single calendarConfig timePickerConfig
    , selectedRange = Nothing
    , isFocused = False
    }


type Msg
    = NoOp
    | PickerMsg DateRangePicker.Msg
    | FocusHandler
    | GlobalClickHandler


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )

        PickerMsg subMsg ->
            let
                ( updated, subCmd, extMsg ) =
                    DateRangePicker.update subMsg model.picker

                selectedRange =
                    case extMsg of
                        DateRangePicker.None ->
                            model.selectedRange

                        DateRangePicker.DateRangeSelected dateRange ->
                            dateRange
            in
            ( { model
                | picker = updated
                , selectedRange = selectedRange
              }
            , Cmd.map PickerMsg subCmd
            )

        FocusHandler ->
            ( { model | isFocused = True }
            , Cmd.none
            )

        GlobalClickHandler ->
            ( { model | isFocused = False }
            , Cmd.none
            )


view : Model -> Html Msg
view { picker, selectedRange, isFocused } =
    let
        dateValue =
            case selectedRange of
                Just { startDate, endDate } ->
                    String.join " - "
                        [ DateExtra.toString (DateTime.getDate startDate)
                        , DateExtra.toString (DateTime.getDate endDate)
                        ]

                Nothing ->
                    ""
    in
    div [ class "section" ]
        [ h3 [] [ text "Single Date Range Picker With Input" ]
        , div [ class "input-group", stopPropagationOn "click" (Decode.succeed ( NoOp, True )) ]
            [ input [ onFocus FocusHandler, value dateValue, readonly True ] []
            , if isFocused then
                Html.map PickerMsg (DateRangePicker.view picker)

              else
                text ""
            ]
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


subscriptions : Model -> Sub Msg
subscriptions { isFocused } =
    if isFocused then
        Browser.onClick (Decode.succeed GlobalClickHandler)

    else
        Sub.none

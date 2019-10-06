module Components.Single.DatePicker exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import DatePicker
import DatePicker.Types exposing (DateLimit(..), ViewType(..))
import DateTime exposing (DateTime)
import Extra.DateTime as DateTimeExtra
import Html exposing (Html, br, div, h3, span, text)
import Html.Attributes exposing (class)
import Time exposing (Posix)


type alias Model =
    { picker : DatePicker.Model
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

        -- The `timePickerConfig` is set to `Nothing` cause we don't want one.
        timePickerConfig =
            Nothing
    in
    { picker = DatePicker.initialise Single calendarConfig timePickerConfig
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
        [ h3 [] [ text "Single Date Picker" ]
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

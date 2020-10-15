module Components.WithInput.Double.DatePicker exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Events as Browser
import DatePicker
import DatePicker.Types exposing (DateLimit(..), ViewType(..))
import DateTime exposing (DateTime)
import Extra.Date as DateExtra
import Extra.DateTime as DateTimeExtra
import Html exposing (Html, br, div, h3, input, span, text)
import Html.Attributes exposing (class, readonly, value)
import Html.Events exposing (onFocus, stopPropagationOn)
import Json.Decode as Decode
import Time exposing (Posix)


type alias Model =
    { picker : DatePicker.Model
    , selectedDateTime : Maybe DateTime
    , isFocused : Bool
    }


init : Time.Posix -> Model
init todayPosix =
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

        -- The `timePickerConfig` is set to `Nothing` cause we don't want one.
        timePickerConfig =
            Nothing
    in
    { picker = DatePicker.initialise Double calendarConfig timePickerConfig
    , selectedDateTime = Nothing
    , isFocused = False
    }


type Msg
    = NoOp
    | PickerMsg DatePicker.Msg
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

        FocusHandler ->
            ( { model | isFocused = True }
            , Cmd.none
            )

        GlobalClickHandler ->
            ( { model | isFocused = False }
            , Cmd.none
            )


view : Model -> Html Msg
view { picker, selectedDateTime, isFocused } =
    let
        dateValue =
            case selectedDateTime of
                Just dateTime ->
                    DateExtra.toString (DateTime.getDate dateTime)

                Nothing ->
                    ""
    in
    div [ class "section" ]
        [ h3 [] [ text "Double Date Picker With Input" ]
        , div [ class "input-group", stopPropagationOn "click" (Decode.succeed ( NoOp, True )) ]
            [ input [ onFocus FocusHandler, value dateValue, readonly True ] []
            , if isFocused then
                Html.map PickerMsg (DatePicker.view picker)

              else
                text ""
            ]
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


subscriptions : Model -> Sub Msg
subscriptions { isFocused } =
    if isFocused then
        Browser.onClick (Decode.succeed GlobalClickHandler)

    else
        Sub.none

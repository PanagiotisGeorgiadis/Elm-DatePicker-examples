module Components.WithInput.Single.DatePicker exposing
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
import Html exposing (Html, div, h3, input, text)
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
        [ h3 [] [ text "Single Date Picker With Input" ]
        , div [ class "input-group", stopPropagationOn "click" (Decode.succeed ( NoOp, True )) ]
            [ input [ onFocus FocusHandler, value dateValue, readonly True ] []
            , if isFocused then
                Html.map PickerMsg (DatePicker.view picker)

              else
                text ""
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.isFocused then
        Browser.onClick (Decode.succeed GlobalClickHandler)

    else
        Sub.none

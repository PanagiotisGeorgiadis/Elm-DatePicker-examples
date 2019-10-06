module Main exposing (main)

import Browser exposing (Document)
import Clock
import DatePicker
import DatePicker.Types exposing (DateLimit(..), ViewType(..))
import DateTime exposing (DateTime)
import Extra.DateTime as DateTimeExtra
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
                                NoLimit
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
        , text (DateTimeExtra.toString model.selectedDateTime)
        ]
    }


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

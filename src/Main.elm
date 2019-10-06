module Main exposing (main)

import Browser exposing (Document)
import Components.Double.DatePicker as DoubleDatePicker
import Components.Double.DateTimePicker as DoubleDateTimePicker
import Components.Single.DatePicker as SingleDatePicker
import Components.Single.DateTimePicker as SingleDateTimePicker
import Html exposing (Html, br, div, text)
import Html.Attributes exposing (class)
import Task
import Time


type alias Flags =
    ()


type alias Model =
    { singleDatePicker : Maybe SingleDatePicker.Model
    , singleDateTimePicker : Maybe SingleDateTimePicker.Model
    , doubleDatePicker : Maybe DoubleDatePicker.Model
    , doubleDateTimePicker : Maybe DoubleDateTimePicker.Model
    }


type Msg
    = Initialise Time.Posix
    | SingleDatePickerMsg SingleDatePicker.Msg
    | SingleDateTimePickerMsg SingleDateTimePicker.Msg
    | DoubleDatePickerMsg DoubleDatePicker.Msg
    | DoubleDateTimePickerMsg DoubleDateTimePicker.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Initialise todayPosix ->
            ( { model
                | singleDatePicker = Just (SingleDatePicker.init todayPosix)
                , singleDateTimePicker = Just (SingleDateTimePicker.init todayPosix)
                , doubleDatePicker = Just (DoubleDatePicker.init todayPosix)
                , doubleDateTimePicker = Just (DoubleDateTimePicker.init todayPosix)
              }
            , Cmd.none
            )

        SingleDatePickerMsg subMsg ->
            case model.singleDatePicker of
                Just datePicker ->
                    let
                        ( subModel, subCmd ) =
                            SingleDatePicker.update subMsg datePicker
                    in
                    ( { model
                        | singleDatePicker = Just subModel
                      }
                    , Cmd.map SingleDatePickerMsg subCmd
                    )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        SingleDateTimePickerMsg subMsg ->
            case model.singleDateTimePicker of
                Just dateTimePicker ->
                    let
                        ( subModel, subCmd ) =
                            SingleDateTimePicker.update subMsg dateTimePicker
                    in
                    ( { model
                        | singleDateTimePicker = Just subModel
                      }
                    , Cmd.map SingleDateTimePickerMsg subCmd
                    )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        DoubleDatePickerMsg subMsg ->
            case model.doubleDatePicker of
                Just datePicker ->
                    let
                        ( subModel, subCmd ) =
                            DoubleDatePicker.update subMsg datePicker
                    in
                    ( { model
                        | doubleDatePicker = Just subModel
                      }
                    , Cmd.map DoubleDatePickerMsg subCmd
                    )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        DoubleDateTimePickerMsg subMsg ->
            case model.doubleDateTimePicker of
                Just dateTimePicker ->
                    let
                        ( subModel, subCmd ) =
                            DoubleDateTimePicker.update subMsg dateTimePicker
                    in
                    ( { model
                        | doubleDateTimePicker = Just subModel
                      }
                    , Cmd.map DoubleDateTimePickerMsg subCmd
                    )

                Nothing ->
                    ( model
                    , Cmd.none
                    )


view : Model -> Document Msg
view model =
    { title = "DatePickers example"
    , body =
        [ div [ class "page" ]
            [ case model.singleDatePicker of
                Just datePicker ->
                    Html.map SingleDatePickerMsg (SingleDatePicker.view datePicker)

                Nothing ->
                    text "Single date picker hasn't been initialised!"
            , case model.singleDateTimePicker of
                Just dateTimePicker ->
                    Html.map SingleDateTimePickerMsg (SingleDateTimePicker.view dateTimePicker)

                Nothing ->
                    text "Single date time picker hasn't been initialised!"
            , case model.doubleDatePicker of
                Just datePicker ->
                    Html.map DoubleDatePickerMsg (DoubleDatePicker.view datePicker)

                Nothing ->
                    text "Double date picker hasn't been initialised!"
            , case model.doubleDateTimePicker of
                Just dateTimePicker ->
                    Html.map DoubleDateTimePickerMsg (DoubleDateTimePicker.view dateTimePicker)

                Nothing ->
                    text "Double date time picker hasn't been initialised!"
            ]
        ]
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { singleDatePicker = Nothing
      , singleDateTimePicker = Nothing
      , doubleDatePicker = Nothing
      , doubleDateTimePicker = Nothing
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

module Main exposing (main)

import Browser exposing (Document)
import Components.Double.DatePicker as DoubleDatePicker
import Components.Double.DateRangePicker as DoubleDateRangePicker
import Components.Double.DateTimePicker as DoubleDateTimePicker
import Components.Single.DatePicker as SingleDatePicker
import Components.Single.DateRangePicker as SingleDateRangePicker
import Components.Single.DateTimePicker as SingleDateTimePicker
import Components.Single.DateTimeRangePicker as SingleDateTimeRangePicker
import Html exposing (Html, br, div, text)
import Html.Attributes exposing (class)
import Task
import Time


type alias Flags =
    ()


type alias Model =
    { singleDatePicker : Maybe SingleDatePicker.Model
    , singleDateTimePicker : Maybe SingleDateTimePicker.Model
    , singleDateRangePicker : Maybe SingleDateRangePicker.Model
    , singleDateTimeRangePicker : Maybe SingleDateTimeRangePicker.Model

    --
    , doubleDatePicker : Maybe DoubleDatePicker.Model
    , doubleDateTimePicker : Maybe DoubleDateTimePicker.Model
    , doubleDateRangePicker : Maybe DoubleDateRangePicker.Model
    }


type Msg
    = Initialise Time.Posix
    | SingleDatePickerMsg SingleDatePicker.Msg
    | SingleDateTimePickerMsg SingleDateTimePicker.Msg
    | SingleDateRangePickerMsg SingleDateRangePicker.Msg
    | SingleDateTimeRangePickerMsg SingleDateTimeRangePicker.Msg
      --
    | DoubleDatePickerMsg DoubleDatePicker.Msg
    | DoubleDateTimePickerMsg DoubleDateTimePicker.Msg
    | DoubleDateRangePickerMsg DoubleDateRangePicker.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Initialise todayPosix ->
            ( { model
                | singleDatePicker = Just (SingleDatePicker.init todayPosix)
                , singleDateTimePicker = Just (SingleDateTimePicker.init todayPosix)
                , singleDateRangePicker = Just (SingleDateRangePicker.init todayPosix)
                , singleDateTimeRangePicker = Just (SingleDateTimeRangePicker.init todayPosix)

                --
                , doubleDatePicker = Just (DoubleDatePicker.init todayPosix)
                , doubleDateTimePicker = Just (DoubleDateTimePicker.init todayPosix)
                , doubleDateRangePicker = Just (DoubleDateRangePicker.init todayPosix)
              }
            , Cmd.none
            )

        SingleDatePickerMsg subMsg ->
            case model.singleDatePicker of
                Just picker ->
                    let
                        ( subModel, subCmd ) =
                            SingleDatePicker.update subMsg picker
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
                Just picker ->
                    let
                        ( subModel, subCmd ) =
                            SingleDateTimePicker.update subMsg picker
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

        SingleDateRangePickerMsg subMsg ->
            case model.singleDateRangePicker of
                Just picker ->
                    let
                        ( subModel, subCmd ) =
                            SingleDateRangePicker.update subMsg picker
                    in
                    ( { model
                        | singleDateRangePicker = Just subModel
                      }
                    , Cmd.map SingleDateRangePickerMsg subCmd
                    )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        SingleDateTimeRangePickerMsg subMsg ->
            case model.singleDateTimeRangePicker of
                Just picker ->
                    let
                        ( subModel, subCmd ) =
                            SingleDateTimeRangePicker.update subMsg picker
                    in
                    ( { model
                        | singleDateTimeRangePicker = Just subModel
                      }
                    , Cmd.map SingleDateTimeRangePickerMsg subCmd
                    )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        DoubleDatePickerMsg subMsg ->
            case model.doubleDatePicker of
                Just picker ->
                    let
                        ( subModel, subCmd ) =
                            DoubleDatePicker.update subMsg picker
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
                Just picker ->
                    let
                        ( subModel, subCmd ) =
                            DoubleDateTimePicker.update subMsg picker
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

        DoubleDateRangePickerMsg subMsg ->
            case model.doubleDateRangePicker of
                Just picker ->
                    let
                        ( subModel, subCmd ) =
                            DoubleDateRangePicker.update subMsg picker
                    in
                    ( { model
                        | doubleDateRangePicker = Just subModel
                      }
                    , Cmd.map DoubleDateRangePickerMsg subCmd
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
                Just picker ->
                    Html.map SingleDatePickerMsg (SingleDatePicker.view picker)

                Nothing ->
                    text "Single date picker hasn't been initialised!"
            , case model.singleDateTimePicker of
                Just picker ->
                    Html.map SingleDateTimePickerMsg (SingleDateTimePicker.view picker)

                Nothing ->
                    text "Single date time picker hasn't been initialised!"
            , case model.singleDateRangePicker of
                Just picker ->
                    Html.map SingleDateRangePickerMsg (SingleDateRangePicker.view picker)

                Nothing ->
                    text "Single date range picker hasn't been initialised!"
            , case model.singleDateTimeRangePicker of
                Just picker ->
                    Html.map SingleDateTimeRangePickerMsg (SingleDateTimeRangePicker.view picker)

                Nothing ->
                    text "Single date-time range picker hasn't been initialised!"
            , case model.doubleDatePicker of
                Just picker ->
                    Html.map DoubleDatePickerMsg (DoubleDatePicker.view picker)

                Nothing ->
                    text "Double date picker hasn't been initialised!"
            , case model.doubleDateTimePicker of
                Just picker ->
                    Html.map DoubleDateTimePickerMsg (DoubleDateTimePicker.view picker)

                Nothing ->
                    text "Double date time picker hasn't been initialised!"
            , case model.doubleDateRangePicker of
                Just picker ->
                    Html.map DoubleDateRangePickerMsg (DoubleDateRangePicker.view picker)

                Nothing ->
                    text "Double date range picker hasn't been initialised!"
            ]
        ]
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { singleDatePicker = Nothing
      , singleDateTimePicker = Nothing
      , singleDateRangePicker = Nothing
      , singleDateTimeRangePicker = Nothing

      --
      , doubleDatePicker = Nothing
      , doubleDateTimePicker = Nothing
      , doubleDateRangePicker = Nothing
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

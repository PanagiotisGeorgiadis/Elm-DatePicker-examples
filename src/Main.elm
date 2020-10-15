module Main exposing (main)

import Browser exposing (Document)
import Components.Double.DatePicker as DoubleDatePicker
import Components.Double.DateRangePicker as DoubleDateRangePicker
import Components.Double.DateTimePicker as DoubleDateTimePicker
import Components.Double.DateTimeRangePicker as DoubleDateTimeRangePicker
import Components.Single.DatePicker as SingleDatePicker
import Components.Single.DateRangePicker as SingleDateRangePicker
import Components.Single.DateTimePicker as SingleDateTimePicker
import Components.Single.DateTimeRangePicker as SingleDateTimeRangePicker
import Components.WithInput.Double.DatePicker as DoubleDatePickerWithInput
import Components.WithInput.Double.DateRangePicker as DoubleDateRangePickerWithInput
import Components.WithInput.Single.DatePicker as SingleDatePickerWithInput
import Components.WithInput.Single.DateRangePicker as SingleDateRangePickerWithInput
import Components.WithInput.Single.DateTimePicker as SingleDateTimePickerWithInput
import Components.WithInput.Single.DateTimeRangePicker as SingleDateTimeRangePickerWithInput
import Html exposing (Html, br, div, text)
import Html.Attributes exposing (class)
import Task
import Time



{- Note: The overall architecture here is only for the purposes
   of the example. You'll usually not have such long names
   for the components nor for the messages.
-}


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
    , doubleDateTimeRangePicker : Maybe DoubleDateTimeRangePicker.Model

    --
    , withInput :
        { singleDatePicker : Maybe SingleDatePickerWithInput.Model
        , singleDateTimePicker : Maybe SingleDateTimePickerWithInput.Model
        , singleDateRangePicker : Maybe SingleDateRangePickerWithInput.Model
        , singleDateTimeRangePicker : Maybe SingleDateTimeRangePickerWithInput.Model

        --
        , doubleDatePicker : Maybe DoubleDatePickerWithInput.Model
        , doubleDateRangePicker : Maybe DoubleDateRangePickerWithInput.Model
        }
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
    | DoubleDateTimeRangePickerMsg DoubleDateTimeRangePicker.Msg
      --
    | SingleDatePickerWithInputMsg SingleDatePickerWithInput.Msg
    | SingleDateTimePickerWithInputMsg SingleDateTimePickerWithInput.Msg
    | SingleDateRangePickerWithInputMsg SingleDateRangePickerWithInput.Msg
    | SingleDateTimeRangePickerWithInputMsg SingleDateTimeRangePickerWithInput.Msg
      --
    | DoubleDatePickerWithInputMsg DoubleDatePickerWithInput.Msg
    | DoubleDateRangePickerWithInputMsg DoubleDateRangePickerWithInput.Msg


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
                , doubleDateTimeRangePicker = Just (DoubleDateTimeRangePicker.init todayPosix)

                --
                , withInput =
                    { singleDatePicker = Just (SingleDatePickerWithInput.init todayPosix)
                    , singleDateTimePicker = Just (SingleDateTimePickerWithInput.init todayPosix)
                    , singleDateRangePicker = Just (SingleDateRangePickerWithInput.init todayPosix)
                    , singleDateTimeRangePicker = Just (SingleDateTimeRangePickerWithInput.init todayPosix)

                    --
                    , doubleDatePicker = Just (DoubleDatePickerWithInput.init todayPosix)
                    , doubleDateRangePicker = Just (DoubleDateRangePickerWithInput.init todayPosix)
                    }
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

        DoubleDateTimeRangePickerMsg subMsg ->
            case model.doubleDateTimeRangePicker of
                Just picker ->
                    let
                        ( subModel, subCmd ) =
                            DoubleDateTimeRangePicker.update subMsg picker
                    in
                    ( { model
                        | doubleDateTimeRangePicker = Just subModel
                      }
                    , Cmd.map DoubleDateTimeRangePickerMsg subCmd
                    )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        SingleDatePickerWithInputMsg subMsg ->
            case model.withInput.singleDatePicker of
                Just picker ->
                    let
                        ( subModel, subCmd ) =
                            SingleDatePickerWithInput.update subMsg picker

                        { withInput } =
                            model

                        updatedWithInput =
                            { withInput | singleDatePicker = Just subModel }
                    in
                    ( { model | withInput = updatedWithInput }
                    , Cmd.map SingleDatePickerWithInputMsg subCmd
                    )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        SingleDateTimePickerWithInputMsg subMsg ->
            case model.withInput.singleDateTimePicker of
                Just picker ->
                    let
                        ( subModel, subCmd ) =
                            SingleDateTimePickerWithInput.update subMsg picker

                        { withInput } =
                            model

                        updatedWithInput =
                            { withInput | singleDateTimePicker = Just subModel }
                    in
                    ( { model | withInput = updatedWithInput }
                    , Cmd.map SingleDateTimePickerWithInputMsg subCmd
                    )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        SingleDateRangePickerWithInputMsg subMsg ->
            case model.withInput.singleDateRangePicker of
                Just picker ->
                    let
                        ( subModel, subCmd ) =
                            SingleDateRangePickerWithInput.update subMsg picker

                        { withInput } =
                            model

                        updatedWithInput =
                            { withInput | singleDateRangePicker = Just subModel }
                    in
                    ( { model | withInput = updatedWithInput }
                    , Cmd.map SingleDateRangePickerWithInputMsg subCmd
                    )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        SingleDateTimeRangePickerWithInputMsg subMsg ->
            case model.withInput.singleDateTimeRangePicker of
                Just picker ->
                    let
                        ( subModel, subCmd ) =
                            SingleDateTimeRangePickerWithInput.update subMsg picker

                        { withInput } =
                            model

                        updatedWithInput =
                            { withInput | singleDateTimeRangePicker = Just subModel }
                    in
                    ( { model | withInput = updatedWithInput }
                    , Cmd.map SingleDateTimeRangePickerWithInputMsg subCmd
                    )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        DoubleDatePickerWithInputMsg subMsg ->
            case model.withInput.doubleDatePicker of
                Just picker ->
                    let
                        ( subModel, subCmd ) =
                            DoubleDatePickerWithInput.update subMsg picker

                        { withInput } =
                            model

                        updatedWithInput =
                            { withInput | doubleDatePicker = Just subModel }
                    in
                    ( { model | withInput = updatedWithInput }
                    , Cmd.map DoubleDatePickerWithInputMsg subCmd
                    )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        DoubleDateRangePickerWithInputMsg subMsg ->
            case model.withInput.doubleDateRangePicker of
                Just picker ->
                    let
                        ( subModel, subCmd ) =
                            DoubleDateRangePickerWithInput.update subMsg picker

                        { withInput } =
                            model

                        updatedWithInput =
                            { withInput | doubleDateRangePicker = Just subModel }
                    in
                    ( { model | withInput = updatedWithInput }
                    , Cmd.none
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

            --
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
            , case model.doubleDateTimeRangePicker of
                Just picker ->
                    Html.map DoubleDateTimeRangePickerMsg (DoubleDateTimeRangePicker.view picker)

                Nothing ->
                    text "Double date-time range picker hasn't been initialised!"

            --
            , case model.withInput.singleDatePicker of
                Just picker ->
                    Html.map SingleDatePickerWithInputMsg (SingleDatePickerWithInput.view picker)

                Nothing ->
                    text "Single date picker with input hasn't been initialised!"
            , case model.withInput.singleDateTimePicker of
                Just picker ->
                    Html.map SingleDateTimePickerWithInputMsg (SingleDateTimePickerWithInput.view picker)

                Nothing ->
                    text "Single date-time picker with input hasn't been initialised!"
            , case model.withInput.singleDateRangePicker of
                Just picker ->
                    Html.map SingleDateRangePickerWithInputMsg (SingleDateRangePickerWithInput.view picker)

                Nothing ->
                    text "Single date range picker with input hasn't been initialised!"
            , case model.withInput.singleDateTimeRangePicker of
                Just picker ->
                    Html.map SingleDateTimeRangePickerWithInputMsg (SingleDateTimeRangePickerWithInput.view picker)

                Nothing ->
                    text "Single date-time range picker with input hasn't been initialised!"

            --
            , case model.withInput.doubleDatePicker of
                Just picker ->
                    Html.map DoubleDatePickerWithInputMsg (DoubleDatePickerWithInput.view picker)

                Nothing ->
                    text "Double date picker with input hasn't been initialised!"
            , case model.withInput.doubleDateRangePicker of
                Just picker ->
                    Html.map DoubleDateRangePickerWithInputMsg (DoubleDateRangePickerWithInput.view picker)

                Nothing ->
                    text "Double date range picker with input hasn't been initialised!"
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
      , doubleDateTimeRangePicker = Nothing

      --
      , withInput =
            { singleDatePicker = Nothing
            , singleDateTimePicker = Nothing
            , singleDateRangePicker = Nothing
            , singleDateTimeRangePicker = Nothing

            --
            , doubleDatePicker = Nothing
            , doubleDateRangePicker = Nothing
            }
      }
    , Task.perform Initialise Time.now
    )


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ case model.withInput.singleDatePicker of
            Just picker ->
                Sub.map SingleDatePickerWithInputMsg (SingleDatePickerWithInput.subscriptions picker)

            Nothing ->
                Sub.none
        , case model.withInput.singleDateTimePicker of
            Just picker ->
                Sub.map SingleDateTimePickerWithInputMsg (SingleDateTimePickerWithInput.subscriptions picker)

            Nothing ->
                Sub.none
        , case model.withInput.singleDateRangePicker of
            Just picker ->
                Sub.map SingleDateRangePickerWithInputMsg (SingleDateRangePickerWithInput.subscriptions picker)

            Nothing ->
                Sub.none
        , case model.withInput.singleDateTimeRangePicker of
            Just picker ->
                Sub.map SingleDateTimeRangePickerWithInputMsg (SingleDateTimeRangePickerWithInput.subscriptions picker)

            Nothing ->
                Sub.none

        --
        , case model.withInput.doubleDatePicker of
            Just picker ->
                Sub.map DoubleDatePickerWithInputMsg (DoubleDatePickerWithInput.subscriptions picker)

            Nothing ->
                Sub.none
        , case model.withInput.doubleDateRangePicker of
            Just picker ->
                Sub.map DoubleDateRangePickerWithInputMsg (DoubleDateRangePickerWithInput.subscriptions picker)

            Nothing ->
                Sub.none
        ]

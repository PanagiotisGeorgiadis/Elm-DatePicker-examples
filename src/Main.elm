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
import Components.WithInput.Double.DateTimePicker as DoubleDateTimePickerWithInput
import Components.WithInput.Double.DateTimeRangePicker as DoubleDateTimeRangePickerWithInput
import Components.WithInput.Single.DatePicker as SingleDatePickerWithInput
import Components.WithInput.Single.DateRangePicker as SingleDateRangePickerWithInput
import Components.WithInput.Single.DateTimePicker as SingleDateTimePickerWithInput
import Components.WithInput.Single.DateTimeRangePicker as SingleDateTimeRangePickerWithInput
import Extra.I18n exposing (Language(..))
import Extra.Time.Weekday
import Html exposing (div, h4, input, label, text)
import Html.Attributes exposing (checked, class, name, type_, value)
import Html.Events exposing (on)
import Json.Decode as Decode
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
        , doubleDateTimePicker : Maybe DoubleDateTimePickerWithInput.Model
        , doubleDateRangePicker : Maybe DoubleDateRangePickerWithInput.Model
        , doubleDateTimeRangePicker : Maybe DoubleDateTimeRangePickerWithInput.Model
        }
    , language : Language
    , startingWeekday : Time.Weekday
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
    | DoubleDateTimePickerWithInputMsg DoubleDateTimePickerWithInput.Msg
    | DoubleDateRangePickerWithInputMsg DoubleDateRangePickerWithInput.Msg
    | DoubleDateTimeRangePickerWithInputMsg DoubleDateTimeRangePickerWithInput.Msg
      --
    | HandleLanguageChangeMsg Language
    | HandleStartingWeekdayChange Time.Weekday


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Initialise todayPosix ->
            ( { model
                | singleDatePicker = Just (SingleDatePicker.init model.language model.startingWeekday todayPosix)
                , singleDateTimePicker = Just (SingleDateTimePicker.init model.language model.startingWeekday todayPosix)
                , singleDateRangePicker = Just (SingleDateRangePicker.init model.language model.startingWeekday todayPosix)
                , singleDateTimeRangePicker = Just (SingleDateTimeRangePicker.init model.language model.startingWeekday todayPosix)

                --
                , doubleDatePicker = Just (DoubleDatePicker.init model.language model.startingWeekday todayPosix)
                , doubleDateTimePicker = Just (DoubleDateTimePicker.init model.language model.startingWeekday todayPosix)
                , doubleDateRangePicker = Just (DoubleDateRangePicker.init model.language model.startingWeekday todayPosix)
                , doubleDateTimeRangePicker = Just (DoubleDateTimeRangePicker.init model.language model.startingWeekday todayPosix)

                --
                , withInput =
                    { singleDatePicker = Just (SingleDatePickerWithInput.init model.language model.startingWeekday todayPosix)
                    , singleDateTimePicker = Just (SingleDateTimePickerWithInput.init model.language model.startingWeekday todayPosix)
                    , singleDateRangePicker = Just (SingleDateRangePickerWithInput.init model.language model.startingWeekday todayPosix)
                    , singleDateTimeRangePicker = Just (SingleDateTimeRangePickerWithInput.init model.language model.startingWeekday todayPosix)

                    --
                    , doubleDatePicker = Just (DoubleDatePickerWithInput.init model.language model.startingWeekday todayPosix)
                    , doubleDateTimePicker = Just (DoubleDateTimePickerWithInput.init model.language model.startingWeekday todayPosix)
                    , doubleDateRangePicker = Just (DoubleDateRangePickerWithInput.init model.language model.startingWeekday todayPosix)
                    , doubleDateTimeRangePicker = Just (DoubleDateTimeRangePickerWithInput.init model.language model.startingWeekday todayPosix)
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

        DoubleDateTimePickerWithInputMsg subMsg ->
            case model.withInput.doubleDateTimePicker of
                Just picker ->
                    let
                        ( subModel, subCmd ) =
                            DoubleDateTimePickerWithInput.update subMsg picker

                        { withInput } =
                            model

                        updatedWithInput =
                            { withInput | doubleDateTimePicker = Just subModel }
                    in
                    ( { model | withInput = updatedWithInput }
                    , Cmd.map DoubleDateTimePickerWithInputMsg subCmd
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

        DoubleDateTimeRangePickerWithInputMsg subMsg ->
            case model.withInput.doubleDateTimeRangePicker of
                Just picker ->
                    let
                        ( subModel, subCmd ) =
                            DoubleDateTimeRangePickerWithInput.update subMsg picker

                        { withInput } =
                            model

                        updatedWithInput =
                            { withInput | doubleDateTimeRangePicker = Just subModel }
                    in
                    ( { model | withInput = updatedWithInput }
                    , Cmd.none
                    )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        HandleLanguageChangeMsg language ->
            ( { model | language = language }
            , Task.perform Initialise Time.now
            )

        HandleStartingWeekdayChange weekday ->
            ( { model | startingWeekday = weekday }
            , Task.perform Initialise Time.now
            )


view : Model -> Document Msg
view model =
    { title = "DatePickers example"
    , body =
        [ div [ class "page" ]
            [ div [ class "picker-row" ]
                [ h4 [] [ text "Select Language" ]
                , div [ class "input-row" ]
                    [ label []
                        [ input
                            [ type_ "radio"
                            , name "language"
                            , value "English"
                            , checked (model.language == English)
                            , on "change" (Decode.succeed (HandleLanguageChangeMsg English))
                            ]
                            []
                        , text "English"
                        ]
                    , label []
                        [ input
                            [ type_ "radio"
                            , name "language"
                            , value "Greek"
                            , checked (model.language == Greek)
                            , on "change" (Decode.succeed (HandleLanguageChangeMsg Greek))
                            ]
                            []
                        , text "Greek"
                        ]
                    ]
                ]
            , div [ class "picker-row" ]
                [ h4 [] [ text "Select a \"starting weekday\" for the calendar" ]
                , div [ class "input-row" ] <|
                    List.map
                        (\weekday ->
                            label []
                                [ input
                                    [ type_ "radio"
                                    , name "starting-weekday"
                                    , value "starting-weekday"
                                    , checked (model.startingWeekday == weekday)
                                    , on "change" (Decode.succeed (HandleStartingWeekdayChange weekday))
                                    ]
                                    []
                                , text (Extra.Time.Weekday.toString weekday)
                                ]
                        )
                        [ Time.Sun, Time.Mon, Time.Tue, Time.Wed, Time.Thu, Time.Fri, Time.Sat ]
                ]
            , case model.singleDatePicker of
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
            , case model.withInput.doubleDateTimePicker of
                Just picker ->
                    Html.map DoubleDateTimePickerWithInputMsg (DoubleDateTimePickerWithInput.view picker)

                Nothing ->
                    text "Single date-time picker with input hasn't been initialised!"
            , case model.withInput.doubleDateRangePicker of
                Just picker ->
                    Html.map DoubleDateRangePickerWithInputMsg (DoubleDateRangePickerWithInput.view picker)

                Nothing ->
                    text "Double date range picker with input hasn't been initialised!"
            , case model.withInput.doubleDateTimeRangePicker of
                Just picker ->
                    Html.map DoubleDateTimeRangePickerWithInputMsg (DoubleDateTimeRangePickerWithInput.view picker)

                Nothing ->
                    text "Double date-time range picker with input hasn't been initialised!"
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
            , doubleDateTimePicker = Nothing
            , doubleDateRangePicker = Nothing
            , doubleDateTimeRangePicker = Nothing
            }
      , language = English
      , startingWeekday = Time.Sun
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
        , case model.withInput.doubleDateTimePicker of
            Just picker ->
                Sub.map DoubleDateTimePickerWithInputMsg (DoubleDateTimePickerWithInput.subscriptions picker)

            Nothing ->
                Sub.none
        , case model.withInput.doubleDateRangePicker of
            Just picker ->
                Sub.map DoubleDateRangePickerWithInputMsg (DoubleDateRangePickerWithInput.subscriptions picker)

            Nothing ->
                Sub.none
        , case model.withInput.doubleDateTimeRangePicker of
            Just picker ->
                Sub.map DoubleDateTimeRangePickerWithInputMsg (DoubleDateTimeRangePickerWithInput.subscriptions picker)

            Nothing ->
                Sub.none
        ]

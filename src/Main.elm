module Main exposing (main)

import Browser exposing (Document)
import Components.DoubleDatePicker as DoubleDatePicker
import Components.SingleDatePicker as SingleDatePicker
import Html exposing (Html, br, text)
import Task
import Time


type alias Flags =
    ()


type alias Model =
    { singleDatePicker : Maybe SingleDatePicker.Model
    , doubleDatePicker : Maybe DoubleDatePicker.Model
    }


type Msg
    = Initialise Time.Posix
    | SingleDatePickerMsg SingleDatePicker.Msg
    | DoubleDatePickerMsg DoubleDatePicker.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Initialise todayPosix ->
            ( { model
                | singleDatePicker = Just (SingleDatePicker.init todayPosix)
                , doubleDatePicker = Just (DoubleDatePicker.init todayPosix)
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


view : Model -> Document Msg
view model =
    { title = "DatePickers example"
    , body =
        [ case model.singleDatePicker of
            Just datePicker ->
                Html.map SingleDatePickerMsg (SingleDatePicker.view datePicker)

            Nothing ->
                text "Single date picker hasn't been initialised!"
        , case model.doubleDatePicker of
            Just datePicker ->
                Html.map DoubleDatePickerMsg (DoubleDatePicker.view datePicker)

            Nothing ->
                text "Double date picker hasn't been initialised!"
        ]
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { singleDatePicker = Nothing
      , doubleDatePicker = Nothing
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

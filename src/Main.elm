module Main exposing (main)

import Browser exposing (Document)
import Components.DatePicker as DatePicker
import Html exposing (Html, br, text)
import Task
import Time


type alias Flags =
    ()


type alias Model =
    { singleDatePicker : Maybe DatePicker.Model
    }


type Msg
    = Initialise Time.Posix
    | DatePickerMsg DatePicker.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Initialise todayPosix ->
            ( { model
                | singleDatePicker = Just (DatePicker.init todayPosix)
              }
            , Cmd.none
            )

        DatePickerMsg subMsg ->
            case model.singleDatePicker of
                Just datePicker ->
                    let
                        ( subModel, subCmd ) =
                            DatePicker.update subMsg datePicker
                    in
                    ( { model
                        | singleDatePicker = Just subModel
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
        [ case model.singleDatePicker of
            Just datePicker ->
                Html.map DatePickerMsg (DatePicker.view datePicker)

            Nothing ->
                text ""
        ]
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { singleDatePicker = Nothing
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

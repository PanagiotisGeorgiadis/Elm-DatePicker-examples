module Extra.Date exposing (toString)

import Calendar


toString : Calendar.Date -> String
toString date =
    String.join "/"
        [ formatDate date
        , formatMonth date
        , String.fromInt (Calendar.getYear date)
        ]


formatDate : Calendar.Date -> String
formatDate date =
    let
        dateInt =
            Calendar.getDay date
    in
    if dateInt < 10 then
        "0" ++ String.fromInt dateInt

    else
        String.fromInt dateInt


formatMonth : Calendar.Date -> String
formatMonth date =
    let
        monthInt =
            Calendar.monthToInt (Calendar.getMonth date)
    in
    if monthInt < 10 then
        "0" ++ String.fromInt monthInt

    else
        String.fromInt monthInt

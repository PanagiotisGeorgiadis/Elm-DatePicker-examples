module Extra.DateTime exposing (toString)

import DateTime exposing (DateTime)
import Extra.Time.Month as MonthExtra
import Extra.Time.Weekday as WeekdayExtra


toString : Maybe DateTime -> String
toString dateTime =
    case dateTime of
        Just dt ->
            String.join " "
                [ String.join " "
                    [ WeekdayExtra.toString (DateTime.getWeekday dt)
                    , String.fromInt (DateTime.getDay dt)
                    , MonthExtra.toString (DateTime.getMonth dt)
                    , String.fromInt (DateTime.getYear dt)
                    ]
                , String.join ":"
                    [ formatTime (DateTime.getHours dt)
                    , formatTime (DateTime.getMinutes dt)
                    , formatTime (DateTime.getSeconds dt)
                    , formatMillis (DateTime.getMilliseconds dt)
                    ]
                ]

        Nothing ->
            "Nothing"


formatTime : Int -> String
formatTime time =
    if time < 10 then
        "0" ++ String.fromInt time

    else
        String.fromInt time


formatMillis : Int -> String
formatMillis millis =
    if millis < 10 then
        "00" ++ String.fromInt millis

    else if millis < 100 then
        "0" ++ String.fromInt millis

    else
        String.fromInt millis

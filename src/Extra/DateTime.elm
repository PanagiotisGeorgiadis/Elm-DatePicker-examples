module Extra.DateTime exposing
    ( decrementDays
    , incrementDays
    , toString
    )

import DateTime exposing (DateTime)
import Extra.Time.Month as MonthExtra
import Extra.Time.Weekday as WeekdayExtra


toString : DateTime -> String
toString dateTime =
    String.join " "
        [ String.join " "
            [ WeekdayExtra.toString (DateTime.getWeekday dateTime)
            , String.fromInt (DateTime.getDay dateTime)
            , MonthExtra.toString (DateTime.getMonth dateTime)
            , String.fromInt (DateTime.getYear dateTime)
            ]
        , String.join ":"
            [ formatTime (DateTime.getHours dateTime)
            , formatTime (DateTime.getMinutes dateTime)
            , formatTime (DateTime.getSeconds dateTime)
            , formatMillis (DateTime.getMilliseconds dateTime) ++ "Z"
            ]
        ]


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


incrementDays : Int -> DateTime -> DateTime
incrementDays days date =
    if days > 0 then
        incrementDays (days - 1) (DateTime.incrementDay date)

    else
        date


decrementDays : Int -> DateTime -> DateTime
decrementDays days date =
    if days > 0 then
        decrementDays (days - 1) (DateTime.decrementDay date)

    else
        date

module DTUtil exposing (..)

{-| Utilities for working with dates & times.
-}

import DTUtil.Month
import Parser exposing ((|.), (|=), Parser)
import ParserUtils
import Time
import Time.Extra


{-| A date.
-}
type alias Date =
    { year : Int
    , month : Time.Month
    , day : Int
    }


{-| A time of day, to minute granularity.
-}
type alias TimeOfDay =
    { hour : Int
    , minute : Int
    }


type alias DateTime =
    { date : Date
    , time : TimeOfDay
    , zone : Time.Zone
    }


toPosix : DateTime -> Time.Posix
toPosix { zone, date, time } =
    let
        { year, month, day } =
            date

        { hour, minute } =
            time
    in
    Time.Extra.partsToPosix zone
        { year = year
        , month = month
        , day = day
        , hour = hour
        , minute = minute
        , second = 0
        , millisecond = 0
        }


fromPosix : Time.Zone -> Time.Posix -> DateTime
fromPosix zone time =
    let
        { year, month, day, hour, minute } =
            Time.Extra.posixToParts zone time
    in
    { zone = zone
    , date =
        { year = year
        , month = month
        , day = day
        }
    , time =
        { hour = hour
        , minute = minute
        }
    }


{-| Parser for dates of the form YYYY-MM-DD
-}
dateParser : Parser Date
dateParser =
    Parser.succeed Date
        |= ParserUtils.decimalWithZeros
        |. Parser.symbol "-"
        |= DTUtil.Month.numParser
        |. Parser.symbol "-"
        |= ParserUtils.decimalWithZeros


{-| Parser for times of the form HH-MM (in 24-hour time).
-}
timeOfDayParser : Parser TimeOfDay
timeOfDayParser =
    Parser.succeed TimeOfDay
        |= ParserUtils.decimalWithZeros
        |. Parser.symbol ":"
        |= ParserUtils.decimalWithZeros

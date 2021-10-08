module FormValues exposing (FormValues)

import Protocol


type alias Range a =
    { start : a
    , stop : a
    }


type alias RepeatOption =
    { freq : String
    , noun : String
    }


repeatOptions =
    [ { freq = "Daily", noun = "day" }
    , { freq = "Weekly", noun = "week" }
    , { freq = "Monthly", noun = "month" }
    , { freq = "Yearly", noun = "year" }
    ]


type alias FormValues =
    { summary : String
    , date : String
    , description : String
    , location : String
    , allDay : Bool
    , time : Range String
    , {- N.B. it would be natural to make time a type like:

         type Time = AllDay | PartOfDay (Range String)

         ...since time isn't meaningful for all day events, but we don't do
         that, because if the user checks 'All Day' and then unchecks it, we
         want the settings to revert to what they were before the check -- which
         means we can't forget about them.

         So, `time` isn't meaningful if `all_day = true` from the perspective of
         the specified event, but we track it anyway for UI purposes.
      -}
      time_zone : String
    , repeat : List Protocol.Repeat
    }


valid m =
    let
        mem s =
            s /= ""
    in
    mem m.summary
        && mem m.date
        && (m.allDay || (mem m.time.start && mem m.time.stop))

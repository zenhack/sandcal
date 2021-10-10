module FormValues exposing
    ( Accessor
    , Flags
    , Model
    , Msg(..)
    , Range
    , RepeatOption
    , chooseTz
    , decodeFlags
    , init
    , makeProtocolNewEvent
    , repeatOptions
    , update
    , valid
    )

import Accessors
import GenAccessors as GA
import Json.Decode as D
import List.Extra
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


type alias Model =
    { summary : String
    , date : String
    , description : String
    , location : String
    , allDay : Bool

    {- N.B. it would be natural to make time a type like:

       type Time = AllDay | PartOfDay (Range String)

       ...since time isn't meaningful for all day events, but we don't do
       that, because if the user checks 'All Day' and then unchecks it, we
       want the settings to revert to what they were before the check -- which
       means we can't forget about them.

       So, `time` isn't meaningful if `all_day = true` from the perspective of
       the specified event, but we track it anyway for UI purposes.
    -}
    , time : Range String
    , timeZone : String
    , repeat : List Protocol.Repeat
    }


type alias Accessor sub super =
    Accessors.Relation sub sub sub -> Accessors.Relation super sub sub


type Msg
    = InputChanged (Accessor String Model) String
    | SetAllDay Bool
    | NewRepeat
    | DeleteRepeat Int
    | Submit


type alias Flags =
    { tpl : Protocol.EditTemplate
    , browserTz : String
    }


decodeFlags : D.Decoder Flags
decodeFlags =
    D.map2 Flags
        (D.field "tpl" Protocol.decodeEditTemplate)
        (D.field "browserTz" D.string)


valid m =
    let
        mem s =
            s /= ""
    in
    mem m.summary
        && mem m.date
        && (m.allDay || (mem m.time.start && mem m.time.stop))


makeProtocolNewEvent fv =
    { summary = fv.summary
    , date = fv.date
    , time =
        if fv.allDay then
            Protocol.AllDay

        else
            Protocol.StartEnd
                { startTime = fv.time.start
                , endTime = fv.time.stop
                , timeZone = fv.timeZone
                }
    , description = fv.description
    , location = fv.location
    , repeats = fv.repeat
    }


update csrf action_ msg model =
    case msg of
        InputChanged rel value ->
            ( Accessors.set rel value model
            , Cmd.none
            )

        SetAllDay value ->
            ( { model | allDay = value }
            , Cmd.none
            )

        NewRepeat ->
            ( model
                |> Accessors.over GA.repeat (\xs -> xs ++ [ { frequency = "Daily", interval = 1 } ])
            , Cmd.none
            )

        DeleteRepeat i ->
            ( model
                |> Accessors.over GA.repeat (List.Extra.removeAt i)
            , Cmd.none
            )

        Submit ->
            Debug.todo "Port from OCaml, below"



{- Old OCaml for the submit case:

   let _ = Protocol.Rpc.postEvent
     ~csrf
     ~action:action_
     (make_protocol_new_event model)
     |> Js.Promise.then_
         (fun r ->
           if not r.Browser.Response.ok then
             failwith "TODO: handle failures"
           else
             begin
               JsFunctions.setLocation r.Browser.Response.url;
               Js.Promise.resolve ()
             end
         )
   in
   -- TODO: mark the state as loading & indicate this to the user somehow?
   model
-}


chooseTz : Flags -> String
chooseTz flags =
    flags.tpl.userTz |> Maybe.withDefault flags.browserTz


init flags =
    let
        userTz =
            chooseTz flags
    in
    let
        defaultTime =
            { start = "12:00"
            , stop = "13:00"
            }
    in
    case flags.tpl.formData of
        Nothing ->
            { summary = ""
            , description = ""
            , location = ""
            , date = Debug.todo "date_prefill_now ()"
            , allDay = False
            , time = defaultTime
            , timeZone = chooseTz flags
            , repeat = []
            }

        Just fd ->
            let
                timeDependent =
                    case fd.time of
                        Protocol.AllDay ->
                            { allDay = True
                            , time = defaultTime
                            , timeZone = userTz
                            }

                        Protocol.StartEnd { startTime, endTime, timeZone } ->
                            { allDay = False
                            , time = { start = startTime, stop = endTime }
                            , timeZone = timeZone
                            }
            in
            { summary = fd.summary
            , description = fd.description
            , location = fd.location
            , date = fd.date
            , allDay = timeDependent.allDay
            , time = timeDependent.time
            , timeZone = timeDependent.timeZone
            , repeat = fd.repeats
            }

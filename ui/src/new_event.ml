open Tea.App
open Tea.Html

open Common

type msg =
  | InputChanged of string * string
  | SetAllDay of bool

module StringMap = Map.Make(String)

module FormValues = struct
  type t = string StringMap.t

  let update (key: string) (value: string) (old: t): t =
    match value with
    | "" -> StringMap.remove key old
    | _ -> StringMap.add key value old

  let init date_prefill =
    [ "Date", date_prefill
    ; "Start Time", "12:00"
    ; "End Time", "13:00"
    ]
    |> List.fold_left
         (fun m (k, v) -> update k v m)
         StringMap.empty

  let all_day (values: t) =
    StringMap.mem "All Day" values

  let valid (values: t) =
    StringMap.mem "Summary" values
      && StringMap.mem "Date" values
      && ( all_day values
            ||
              ( StringMap.mem "Start Time" values
              && StringMap.mem "End Time" values
            )
        )
end

type model = {
  user_tz: string option;
  date_prefill: string;
  form_values: FormValues.t;
  action_: string;
}

let init user_tz action_ =
  let user_tz = match user_tz with
    | "" -> None
    | tz -> Some tz
  in
  let date_prefill = Js.Date.(
        let now = make () in
        let str n =
          let ret = string_of_int (int_of_float n) in
          if n < 10. then
            ("0" ^ ret)
          else
            ret
        in
        str (getFullYear now)
          ^ "-"
          ^ str (getMonth now +. 1.)
          ^ "-"
          ^ str (getDate now)
      )
  in
  { form_values = FormValues.init date_prefill
  ; date_prefill
  ; user_tz
  ; action_
  }

let update model = function
  | InputChanged (key, value) ->
      { model
        with form_values = FormValues.update key value model.form_values
      }
  | SetAllDay value ->
      { model
        with form_values =
          if value then
              FormValues.update "All Day" "on" model.form_values
          else
              FormValues.update "All Day" "" model.form_values
      }

let view model =
  let tracked_input key attrs =
    let event = onInput (fun value -> InputChanged(key, value)) in
    labeled_input key (event :: attrs)
  in
  form
    [ method' "post"; action model.action_ ]
    [ form_block (
        [ tracked_input "Summary" []
        ; tracked_input "Date" [ type' "date"; value model.date_prefill ]
        ; labeled_input "All Day"
            [ onCheck (fun value -> SetAllDay(value))
            ; type' "checkbox"
            ]
        ] @
        (if FormValues.all_day model.form_values then
          []
        else
          [ tracked_input "Start Time" [ type' "time"; value "12:00" ]
          ; tracked_input "End Time" [ type' "time"; value "13:00" ]
          ; labeled_tz_select "Time Zone" model.user_tz
          ]
        )
        @
        [ labeled_select "Repeats"
              (("Never", true)
               :: List.map
                 (fun name -> (name, false))
                 ["Daily"; "Weekly"; "Monthly"; "Yearly"])
        ]
      )
    ; button
        [ type' "submit"
        ; Attributes.disabled (not (FormValues.valid model.form_values))
        ]
        [ text "Create" ]
    ]

let main user_tz action_ =
  beginnerProgram {
    model = init user_tz action_;
    update;
    view;
  }

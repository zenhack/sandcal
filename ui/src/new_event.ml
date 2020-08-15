open Tea.App
open Tea.Html

open Common

let date_prefill_now () = Js.Date.(
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

  let init tpl =
    let user_tz = match tpl.Protocol.EditTemplate.user_tz with
      | None -> []
      | Some tz -> [ "Time Zone", tz ]
    in
    let fields =
      match tpl.Protocol.EditTemplate.form_data with
      | None ->
          [ "Date", date_prefill_now ()
          ; "Start Time", "12:00"
          ; "End Time", "13:00"
          ]
          @
          user_tz
      | Some fd -> List.concat [
          [ "Summary", fd.summary
          ; "Description", fd.description
          ; "Location", fd.location
          ; "Date", fd.date
          ];
          begin match fd.time with
            | AllDay ->
                [ "All Day", "on"
                ]
                @ user_tz
            | StartEnd {start_time; end_time; time_zone} ->
                [ "Start Time", start_time
                ; "End Time", end_time
                ; "Time Zone", time_zone
                ]
          end;
          begin match fd.repeats with
            | None -> []
            | Some v -> [ "Repeats", v ]
          end;
        ]
    in
    List.fold_left
      (fun m (k, v) -> update k v m)
      StringMap.empty
      fields

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

  let date (values: t) =
    StringMap.find "Date" values
end

type model = {
  user_tz: string option;
  form_values: FormValues.t;
  form_values_init: FormValues.t;
  action_: string;
  submit_text: string;
}

let init tpl =
  let Protocol.EditTemplate.{user_tz; action = action_; submit_text; form_data = _} = tpl in
  let form_values = FormValues.init tpl  in
  { form_values
  ; form_values_init = form_values
  ; user_tz
  ; action_
  ; submit_text
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
  let tracked_textarea key =
    let event = onInput (fun value -> InputChanged(key, value)) in
    labeled_elem textarea key [event] []
  in
  let tracked_input key attrs =
    let event = onInput (fun value -> InputChanged(key, value)) in
    labeled_input key (event :: attrs)
  in
  form
    [ method' "post"; action model.action_ ]
    [ form_block (
        [ tracked_input "Summary" []
        ; tracked_input "Date" [ type' "date"; value (FormValues.date model.form_values_init) ]
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
        ; tracked_input "Location" []
        ; tracked_textarea "Description"
        ]
      )
    ; button
        [ type' "submit"
        ; Attributes.disabled (not (FormValues.valid model.form_values))
        ]
        [ text model.submit_text ]
    ]

let main tpl =
  let tpl = Js.Json.parseExn tpl in
  let tpl = Protocol.EditTemplate.decode tpl in
  let tpl = match tpl with
    | None -> failwith "decode failed"
    | Some v -> v
  in
  beginnerProgram {
    model = init tpl;
    update;
    view;
  }

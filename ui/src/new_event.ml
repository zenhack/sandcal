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

let choose_tz user_tz browser_tz = match user_tz with
  | Some tz -> tz
  | None -> browser_tz

module FormValues = struct
  type t = string StringMap.t

  let update (key: string) (value: string) (old: t): t =
    match value with
    | "" -> StringMap.remove key old
    | _ -> StringMap.add key value old

  let init tpl browser_tz =
    let user_tz = [ "Time Zone", choose_tz tpl.Protocol.EditTemplate.user_tz browser_tz ] in
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

  let get_exn (values: t) k =
    StringMap.find k values

  let get (values: t) k =
    StringMap.find_opt k values

  let get_or (values: t) k default =
    match get values k with
    | Some v -> v
    | None -> default
end

type model = {
  user_tz: string;
  form_values: FormValues.t;
  form_values_init: FormValues.t;
  action_: string;
  submit_text: string;
  csrf_token: string;
}

let init tpl browser_tz =
  let Protocol.EditTemplate.{user_tz; action = action_; submit_text; csrf_token; form_data = _} = tpl in
  let form_values = FormValues.init tpl browser_tz in
  { form_values
  ; form_values_init = form_values
  ; user_tz = choose_tz user_tz browser_tz
  ; action_
  ; submit_text
  ; csrf_token
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
  let vals = model.form_values_init in
  let tracked_textarea key content =
    let event = onInput (fun value -> InputChanged(key, value)) in
    labeled_elem textarea key [event; value content] []
  in
  let tracked_input key attrs =
    let event = onInput (fun value -> InputChanged(key, value)) in
    labeled_input key (event :: attrs)
  in
  form
    [ method' "post"; action model.action_ ]
    [ form_block (
        [ input' [ type' "hidden"; name "csrfToken"; value model.csrf_token ] []
        ; tracked_input "Summary" [ value (FormValues.get_or vals "Summary" "") ]
        ; tracked_input "Date" [ type' "date"; value (FormValues.date vals) ]
        ; labeled_input "All Day"
            [ onCheck (fun value -> SetAllDay(value))
            ; type' "checkbox"
            ]
        ] @
        (if FormValues.all_day model.form_values then
          []
        else
          [ tracked_input "Start Time" [ type' "time"; value (FormValues.get_exn vals "Start Time") ]
          ; tracked_input "End Time" [ type' "time"; value (FormValues.get_exn vals "End Time") ]
          ; labeled_tz_select "Time Zone" (FormValues.get vals "Time Zone")
          ]
        )
        @
        [
          begin
            let repeats = FormValues.get vals "Repeats" in
            labeled_select "Repeats"
              (( "Never"
               , match repeats with
                  | Some _ -> false
                  | None -> true
               )
                 :: List.map
                   (fun name ->
                       match repeats with
                       | Some v -> (name, String.equal name v)
                       | None -> (name, false)
                   )
                   ["Daily"; "Weekly"; "Monthly"; "Yearly"]
                 )
          end
        ; tracked_input "Location" [ value (FormValues.get_or vals "Location" "") ]
        ; tracked_textarea "Description" (FormValues.get_or vals "Description" "")
        ]
      )
    ; button
        [ type' "submit"
        ; Attributes.disabled (not (FormValues.valid model.form_values))
        ]
        [ text model.submit_text ]
    ]

let main tpl browser_tz =
  let tpl = Js.Json.parseExn tpl in
  let tpl = Protocol.EditTemplate.decode tpl in
  let tpl = match tpl with
    | None -> failwith "decode failed"
    | Some v -> v
  in
  beginnerProgram {
    model = init tpl browser_tz;
    update;
    view;
  }

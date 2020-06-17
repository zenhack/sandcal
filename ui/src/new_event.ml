open Tea.App
open Tea.Html

open Common

type msg =
  | InputChanged of string * string

module StringMap = Map.Make(String)

module FormValues = struct
  type t = string StringMap.t

  let init = StringMap.empty

  let update (key: string) (value: string) (old: t): t =
    match value with
    | "" -> StringMap.remove key old
    | _ -> StringMap.add key value old

  let valid (values: t) =
    StringMap.mem "Summary" values
end

type model = {
  user_tz: string option;
  form_values: FormValues.t;
}

let init user_tz =
  let user_tz = match user_tz with
    | "" -> None
    | tz -> Some tz
  in
  { form_values = FormValues.init
  ; user_tz
  }

let update model = function
  | InputChanged (key, value) ->
      { model
        with form_values = FormValues.update key value model.form_values
      }

let view model =
  let tracked_input ?typ key =
    let event = onInput (fun value -> InputChanged(key, value)) in
    match typ with
    | None -> labeled_input key [ event ]
    | Some typ -> labeled_input key [ type' typ; event ]
  in
  form
    [ method' "post"; action "/event/new" ]
    [ form_block
        [ tracked_input "Summary"
        ; tracked_input "Date" ~typ:"date"
        ; tracked_input "Start Time" ~typ:"time"
        ; tracked_input "End Time" ~typ:"time"
        ; labeled_tz_select "Time Zone" model.user_tz
        ; labeled_select "Repeats"
              (("Never", true)
               :: List.map
                 (fun name -> (name, false))
                 ["Daily"; "Weekly"; "Monthly"; "Yearly"])
        ]
    ; button
        [ type' "submit"
        ; Attributes.disabled (not (FormValues.valid model.form_values))
        ]
        [ text "Create" ]
    ]

let main user_tz =
  beginnerProgram {
    model = init user_tz;
    update;
    view;
  }

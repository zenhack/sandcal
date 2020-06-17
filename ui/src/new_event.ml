open Tea.App
open Tea.Html

open Common

type msg = unit

module FormValues = struct
  type t = {
    date: string;
    start_time : string;
    end_time : string;
  }

  let init : t = {
    date = "";
    start_time = "";
    end_time = "";
  }

  let valid (values: t) =
    true
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

let update model () = model

let view model =
  form
    [ method' "post"; action "/event/new" ]
    [ form_block
        [ labeled_input "Summary" []
        ; labeled_input "Date" [ type' "date" ]
        ; labeled_input "Start Time" [ type' "time" ]
        ; labeled_input "End Time" [ type' "time" ]
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

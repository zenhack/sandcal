open Tea.App
open Tea.Html

open Common

type msg = unit

type model = {
  user_tz: string option;
}

let init = function
  | "" -> { user_tz = None }
  | tz -> { user_tz = Some tz }

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
    ; button [ type' "submit" ] [ text "Create" ]
    ]

let main user_tz =
  beginnerProgram {
    model = init user_tz;
    update;
    view;
  }

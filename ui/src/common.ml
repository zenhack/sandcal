open Tea.Html

let labeled_elem elem label_name attrs kids =
  div
      [ class' "labeledInput" ]
      [ label [ for' label_name ] [ text label_name ]
      ; elem (name label_name :: attrs) kids
      ]

let labeled_input ~typ label_name attrs =
  labeled_elem input' label_name (type' typ :: attrs) []

let labeled_select select_name options =
  labeled_elem
    select
    select_name
    []
    (List.map
        (fun (name, selected) ->
          option'
            [ value name
            ; Attributes.selected selected
            ]
            [ text name ]
        )
        options
    )

let labeled_tz_select select_name user_tz =
  labeled_select select_name
    (List.map
        (fun tzname ->
            ( tzname
            (* Current versions of the ocaml stdlib have Option.equal, but
               not 4.06, which is where bucklescript is currently pinned. *)
            , match user_tz with
              | None -> false
              | Some user_tzname -> String.equal user_tzname tzname
            )
        )
       Gen_tz.tz_labels
    )

let form_block body =
  div [ class' "formBlock" ] body

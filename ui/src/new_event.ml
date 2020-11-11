open Tea.App
open Tea.Html

open Common

let choose_tz user_tz browser_tz = match user_tz with
  | Some tz -> tz
  | None -> browser_tz

module FormValues = struct
  type 'a range = {
    start: 'a;
    stop: 'a;
  }

  module Repeat = struct
    type t = string

    let never = "Never"

    let options = [
      "Daily";
      "Weekly";
      "Monthly";
      "Yearly";
    ]
  end

  type t = {
    summary: string;
    date: string;

    description: string;
    location: string;

    all_day: bool;
    time: string range;
    (* N.B. it would be natural to do something like:

       time: [ `AllDay | `PartOfDay of string Range.t ]

       ...since time isn't meaningful for all day events, but we don't do
       that, because if the user checks 'All Day' and then unchecks it, we
       want the settings to revert to what they were before the check -- which
       means we can't forget about them.

       So, `time` isn't meaningful if `all_day = true` from the perspective of
       the specified event, but we track it anyway for UI purposes.
    *)

    time_zone: string;
    repeat: Repeat.t;
  }

  let valid m =
    let mem s = not (String.equal s "") in
    mem m.summary
      && mem m.date
      && (m.all_day || (mem m.time.start && mem m.time.stop))

  module Lenses = struct
    (* TODO: automate all this boilerplate *)
    module Range = struct
      let start : ('a range, 'a) Lens.t =
        fun {start; stop} -> Lens.{
          get = (fun () -> start);
          set = (fun start -> {start; stop});
        }

      let stop : ('a range, 'a) Lens.t =
        fun {start; stop} -> Lens.{
          get = (fun () -> stop);
          set = (fun stop -> {start; stop});
        }
    end

    let summary : (t, string) Lens.t =
      fun m -> Lens.{
          get = (fun () -> m.summary);
          set = (fun s -> { m with summary = s });
        }

    let description : (t, string) Lens.t =
      fun m -> Lens.{
          get = (fun () -> m.description);
          set = (fun s -> { m with description = s });
        }

    let location : (t, string) Lens.t =
      fun m -> Lens.{
          get = (fun () -> m.location);
          set = (fun s -> { m with location = s });
        }

    let date : (t, string) Lens.t =
      fun m -> Lens.{
          get = (fun () -> m.date);
          set = (fun d -> { m with date = d});
        }

    let all_day : (t, bool) Lens.t =
      fun m -> Lens.{
          get = (fun () -> m.all_day);
          set = (fun v -> { m with all_day = v });
        }

    let time : (t, string range) Lens.t =
      fun m -> Lens.{
          get = (fun () -> m.time);
          set = (fun v -> { m with time = v });
        }

    let time_zone : (t, string) Lens.t =
      fun m -> Lens.{
          get = (fun () -> m.time_zone);
          set = (fun v -> { m with time_zone = v });
        }

    let repeat : (t, Repeat.t) Lens.t =
      fun m -> Lens.{
          get = (fun () -> m.repeat);
          set = (fun v -> { m with repeat = v });
        }
  end

  type msg =
    | InputChanged of ((t, string) Lens.t * string)
    | SetAllDay of bool

  let update model = function
    | InputChanged (lens, value) ->
        Lens.set lens model value
    | SetAllDay value ->
        { model with all_day = value }

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

  let init tpl browser_tz =
      let user_tz = choose_tz tpl.Protocol.EditTemplate.user_tz browser_tz in
      let default_time = {
            start = "12:00";
            stop = "13:00";
        }
      in
      match tpl.Protocol.EditTemplate.form_data with
        | None -> {
            summary = "";
            description = "";
            location = "";
            date = date_prefill_now ();
            all_day = false;
            time = default_time;
            time_zone = user_tz;
            repeat = Repeat.never;
          }
        | Some fd -> {
            summary = fd.summary;
            description = fd.description;
            location = fd.location;
            date = fd.date;
            all_day = (match fd.time with AllDay -> true | _ -> false);
            time =
              begin match fd.time with
              | AllDay -> default_time
              | StartEnd {start_time; end_time; _} -> {
                  start = start_time;
                  stop = end_time;
                }
              end;
            time_zone =
              begin match fd.time with
                | AllDay -> user_tz
                | StartEnd {time_zone; _} -> time_zone
              end;
            repeat =
              begin match fd.repeats with
                | [] -> Repeat.never
                | [v] -> v.frequency
              end;
          }
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

module Lenses = struct
  let form_values : (model, FormValues.t) Lens.t =
    fun m -> Lens.{
        get = (fun () -> m.form_values);
        set = (fun fv -> { m with form_values = fv })
      }
end

let update model msg =
  Lens.modify Lenses.form_values model (fun fv -> FormValues.update fv msg)

let view model =
  let module Lenses = FormValues.Lenses in
  let fv = model.form_values in
  let tracked_textarea key lens =
    let content = Lens.get lens fv in
    let event = onInput (fun value -> FormValues.InputChanged(lens , value)) in
    labeled_elem textarea key [event; value content] []
  in
  let tracked_input ~typ key lens =
    let event = onInput (fun value -> FormValues.InputChanged(lens, value)) in
    labeled_input ~typ key [event; value (Lens.get lens fv)]
  in
  form
    [ method' "post"; action model.action_ ]
    [ form_block (
        [ input' [ type' "hidden"; name "csrfToken"; value model.csrf_token ] []
        ; tracked_input ~typ:"text" "Summary" FormValues.Lenses.summary
        ; tracked_input ~typ:"date" "Date" FormValues.Lenses.date
        ; labeled_input ~typ:"checkbox" "All Day" [ onCheck (fun value -> FormValues.SetAllDay(value)) ]
        ] @
        (if model.form_values.all_day then
          []
        else
          [ tracked_input
              ~typ:"time"
              "Start Time"
              Lens.(Lenses.(time >>> Range.start))
          ; tracked_input
              ~typ:"time"
              "End Time"
              Lens.(Lenses.(time >>> Range.stop))
          ; labeled_tz_select "Time Zone" (Some fv.time_zone)
          ]
        )
        @
        [
          begin
            labeled_select "Repeats"
              (List.map
                (fun name -> (name, String.equal name fv.repeat))
                FormValues.Repeat.(never :: options)
              )
          end
        ; tracked_input ~typ:"text" "Location" Lenses.location
        ; tracked_textarea "Description" Lenses.description
        ]
      )
    ; button
        [ type' "submit"
        ; Attributes.disabled (not (FormValues.valid fv))
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

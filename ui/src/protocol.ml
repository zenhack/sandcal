
let (>>=) = fun x f -> match x with
  | None -> None
  | Some v -> f v

module Frequency = struct
  type t = string (* TODO: make this a variant. *)

  let decode: Js.Json.t -> t option =
    Js.Json.decodeString
end

module NewEvent = struct
  module Time = struct
    type t =
      | AllDay
      | StartEnd of {
          start_time: string;
          end_time: string;
          time_zone: string;
        }
  end
  type t = {
    summary: string;
    description: string;
    location: string;
    date: string;
    time: Time.t;
    repeats: Frequency.t option;
  }

  let decode: Js.Json.t -> t option =
    fun v ->
      let open Js.Json in
      let get = Js.Dict.get in
      decodeObject v >>= fun obj ->
      get obj "summary" >>= decodeString >>= fun summary ->
      get obj "description" >>= decodeString >>= fun description ->
      get obj "location" >>= decodeString >>= fun location ->
      get obj "date" >>= decodeString >>= fun date ->
      get obj "time" >>= Time.decode >>= fun time ->
      get obj "repeats" >>= fun r ->
        let repeats = Frequency.decode r in
        Some {
          summary;
          description;
          location;
          date;
          time;
          repeats;
        }
end

module EditTemplate = struct
  type t = {
    title: string;
    submit_text: string;
    user_tz: string option;
    action: string;
    form_data: NewEvent.t option;
  }

  let decode: Js.Json.t -> t option =
    let open Js.Json in
    let get = Js.Dict.get in
    fun t -> decodeObject t >>= fun obj ->
    get obj "title" >>= decodeString >>= fun title ->
    get obj "submitText" >>= decodeString >>= fun submit_text ->
    get obj "action" >>= decodeString >>= fun action ->
    (match get obj "userTz" with
      | None -> Some None
      | Some v -> Some (decodeString v)
    )
    >>= (fun user_tz ->
      (match get obj "formData" with
        | None -> Some None
        | Some v -> NewEvent.decode v
      ) >>= (fun form_data ->
        Some {title; submit_text; user_tz; action; form_data}
      )
    )
end

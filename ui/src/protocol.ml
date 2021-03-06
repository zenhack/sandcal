
let (>>=) = fun x f -> match x with
  | None -> None
  | Some v -> f v

let encodeFields fields =
  Js.Json.object_ (Js.Dict.fromList fields)

let decodeList decodeElem v =
  Js.Json.(
    decodeArray v >>= fun arr ->
    Array.fold_right
      (fun item tail ->
          tail >>= fun xs ->
          decodeElem item >>= fun x ->
          Some (x :: xs)
      )
      arr
      (Some [])
  )

module Frequency = struct
  type t = string (* TODO: make this a variant. *)

  let encode: t -> Js.Json.t =
    Js.Json.string

  let decode: Js.Json.t -> t option =
    Js.Json.decodeString
end

module Repeat = struct
  type t = {
    frequency: Frequency.t;
    interval: int;
  }

  let encode v =
    encodeFields [
      ("frequency", Frequency.encode v.frequency);
      ("interval", Js.Json.number (float_of_int v.interval));
    ]

  let decode: Js.Json.t -> t option =
    fun v ->
    let open Js.Json in
    let get = Js.Dict.get in
    decodeObject v >>= fun obj ->
    get obj "frequency" >>= decodeString >>= fun frequency ->
    get obj "interval" >>= decodeNumber >>= fun num ->
    let interval = int_of_float num in
    Some { frequency; interval }

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

    let encode = function
      | AllDay -> encodeFields [("tag", Js.Json.string "AllDay")]
      | StartEnd {start_time; end_time; time_zone} ->
          let s = Js.Json.string in
          encodeFields [
            ("tag", s "StartEnd");
            ("startTime", s start_time);
            ("endTime", s end_time);
            ("timeZone", s time_zone);
          ]

    let decode v =
      let open Js.Json in
      let get = Js.Dict.get in
      decodeObject v >>= fun obj ->
      get obj "tag" >>= decodeString >>= function
        | "AllDay" -> Some AllDay
        | "StartEnd" -> (
            get obj "startTime" >>= decodeString >>= fun start_time ->
            get obj "endTime" >>= decodeString >>= fun end_time ->
            get obj "timeZone" >>= decodeString >>= fun time_zone ->
            Some (StartEnd {
                start_time;
                end_time;
                time_zone;
              })
          )
        | _ ->
            None
  end
  type t = {
    summary: string;
    description: string;
    location: string;
    date: string;
    time: Time.t;
    repeats: Repeat.t list;
  }

  let encode v =
    let s = Js.Json.string in
    encodeFields [
      ("summary", s v.summary);
      ("description", s v.description);
      ("location", s v.location);
      ("date", s v.date);
      ("time", Time.encode v.time);
      ( "repeats"
      , v.repeats
          |> List.map Repeat.encode
          |> Array.of_list
          |> Js.Json.array
      )
    ]

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
      decodeList Repeat.decode r >>= fun repeats ->
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
    csrf_token: string;
  }

  let decode: Js.Json.t -> t option =
    let open Js.Json in
    let get = Js.Dict.get in
    fun t -> decodeObject t >>= fun obj ->
    get obj "title" >>= decodeString >>= fun title ->
    get obj "submitText" >>= decodeString >>= fun submit_text ->
    get obj "action" >>= decodeString >>= fun action ->
    get obj "csrfToken" >>= decodeString >>= fun csrf_token ->
    (match get obj "userTz" with
      | None -> Some None
      | Some v -> Some (decodeString v)
    )
    >>= (fun user_tz ->
      (match get obj "formData" with
        | None -> None
        | Some v ->
            begin match classify v with
              | JSONNull -> Some None
              | _ -> NewEvent.decode v >>= fun ev -> Some (Some ev)
            end
      ) >>= (fun form_data ->
        Some {title; submit_text; user_tz; action; form_data; csrf_token}
      )
    )
end

module Rpc = struct
  let postEvent ~csrf ~action ev =
    NewEvent.encode ev
    |> Js.Json.stringify
    |> JsFunctions.postJsonWithCSRF action csrf
end

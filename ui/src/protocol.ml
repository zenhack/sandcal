

module EditTemplate = struct
  type t = {
    title: string;
    submit_text: string;
    user_tz: string option;
    action: string;
  }

  let decode: Js.Json.t -> t option =
    let (>>=) = fun x f -> match x with
      | None -> None
      | Some v -> f v
    in
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
        Some {title; submit_text; user_tz; action}
    )
end


module Headers = struct
  type t

  external get : t -> string -> string Js.Nullable.t = "get" [@@bs.send]
end

module Response = struct
  type t = {
    headers: Headers.t;
    ok: bool;
    redirected: bool;
    status: int;
    statusText: string;
    url: string;
  }
end

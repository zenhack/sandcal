
external postJsonWithCSRF
  : string -> string -> string
  -> Browser.Response.t Js.Promise.t
  = "postJsonWithCSRF" [@@bs.val]

external setLocation : string -> unit = "setLocation" [@@bs.val]

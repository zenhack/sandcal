
type ('container, 'item) result = {
  get : unit -> 'item;
  set : 'item -> 'container;
}

type ('container, 'item) t = 'container -> ('container, 'item) result

let set : ('c, 'a) t -> 'c -> 'a -> 'c =
  fun l c a -> (l c).set a

let get : ('c, 'a) t -> 'c -> 'a =
  fun l c -> (l c).get ()

let modify : ('c, 'a) t -> 'c -> ('a -> 'a) -> 'c =
  fun l c f ->
    let {set; get} = l c in
    set (f (get ()))

let compose f g = fun c ->
  let g_res = g c in
  let g_val = g_res.get () in
  let f_res = f g_val in
  {
    get = f_res.get;
    set = (fun v -> g_res.set (f_res.set v));
  }

let ( <<< ) f g = compose f g
let ( >>> ) g f = compose f g

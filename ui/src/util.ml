
module List = struct
  let split_at n xs =
    let rec go acc n xs = match n, xs with
      | 0, _ | _, [] -> (List.rev acc, xs)
      | _, (y::ys) -> go (y :: acc) (n - 1) ys
    in
    go [] n xs

  let delete_nth n xs =
    match split_at n xs with
    | (ys, (z :: zs)) -> ys @ zs
    | _ -> xs
end

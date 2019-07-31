exception Finally of exn * exn

let protectx ~f:f a ~finally:finally =
  let finally' up a = try finally a with | ex -> raise (Finally (up, ex)) in
  let b =
    try f a  with
    | ex -> finally' ex a ; raise ex
  in finally a ; b

let protect ~f ~finally = protectx ~f () ~finally


type ('a, 'e) t = ('a, 'e) result

let ok v = Ok v

let error e = Error e

let value r ~default =
  match r with
  | Ok v -> v
  | _ -> default

let get_ok = function
  | Ok v -> v
  | _ -> invalid_arg "Error value passed to get_ok"

let get_error = function
  | Error e -> e
  | _ -> invalid_arg "Ok value passed to get_error"

let bind r f =
  match r with
  | Ok v -> f v
  | Error e -> Error e

let join = function
  | Ok r -> r
  | Error e -> Error e

let map f = function
  | Ok v -> Ok (f v)
  | Error e -> Error e

let map_error f = function
  | Ok v -> Ok v
  | Error e -> Error (f e)

let fold ~ok ~error = function
  | Ok v -> ok v
  | Error e -> error e

let iter c = function
  | Ok v -> c v
  | _ -> ()

let iter_error c = function
  | Error e -> c e
  | _ -> ()

let is_ok = function Ok _ -> true | Error _ -> false

let is_error = function Ok _ -> false | Error _ -> true

let equal ~ok ~error r1 r2 =
  match r1, r2 with
  | Ok _, Error _ | Error _, Ok _ -> false
  | Ok v1, Ok v2 -> ok v1 v2
  | Error e1, Error e2 -> error e1 e2

let compare ~ok ~error r1 r2 =
  match r1, r2 with
  | Ok _, Error _ -> -1
  | Error _, Ok _ -> 1
  | Ok v1, Ok v2 -> ok v1 v2
  | Error e1, Error e2 -> error e1 e2

let to_option = function
  | Ok v -> Some v
  | _ -> None

let to_list = function
  | Ok v -> [v]
  | _ -> []

let to_seq = function
  | Ok v -> Seq.return v
  | _ -> Seq.empty

let ljoin (type e) l =
  let module J = struct exception Join of e end in
  let f a = function
    | Ok v -> v :: a
    | Error e -> raise (J.Join e) in
  try Ok (List.fold_left f [] l) with J.Join e -> Error e

let rec lfold f b = function
  | [] -> Ok b
  | av :: avs ->
     ( match f b av with
       | Ok b' -> lfold f b' avs
       | Error _ as e -> e )

module type MAPS =
  sig

  module M : Map.S
  val mjoin : ('a, 'e) t M.t -> ('a M.t, 'e) t
  val mfold : ('b -> M.key -> 'a -> ('b, 'e) t) -> 'b -> 'a M.t -> ('b, 'e) t

  end

module Maps (M': Map.S) =
  struct

  module M = M'

  let mjoin (type e) m =
    let module J = struct exception Join of e end in
    let f k r a = match r with
    | Ok v -> M.add k v a
    | Error e -> raise (J.Join e) in
    try Ok (M.fold f m (M.empty)) with J.Join e -> Error e

  let mfold (type e) f b m =
    let module F = struct exception Fold of e end in
    let f' k a b' = match f b' k a with
    | Ok b'' -> b''
    | Error e -> raise (F.Fold e) in
    try Ok (M.fold f' m b) with F.Fold e -> Error e

  end

module type MONAD =
  sig

  val return : 'a -> ('a, 'e) t

  val ( >>= ) : ('a, 'e) t ->
                ('a -> ('b, 'e) t) -> ('b, 'e) t

  val ( >>| ) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t
  end

module Monad : MONAD =
  struct

  let return = ok

  let ( >>= ) = bind

  let ( >>| ) a f = map f a

  end

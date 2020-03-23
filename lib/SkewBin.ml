(** type of skew binary integers: list of nonzero weights in increasing order *)
type t = int list

(** Abstraction is cool. :-(|) *)
let weights s = s

(** Get the integer value of a skew binary number *)
let to_int s = List.fold_left (+) 0 s

(** Increment a skew binary number *)
let inc = function
  | w1 :: w2 :: ws when w1 = w2 -> (w1 + w2 + 1) :: ws
  | ws -> 1 :: ws

(** Decrement a skew binary number *)
let dec = function
  | [] -> invalid_arg "SkewBinary.dec"
  | 1 :: ws -> ws
  | w :: ws -> let w' = w / 2 in w' :: w' :: ws

let rec inc' i ws = match i with 0 -> ws | i -> inc' (i - 1) (inc ws)

let bit_positions i =
  let rec bit_positions' a p = function
  | 0 -> a
  | i ->
     let a1 = if i mod 2 = 0 then a else p :: a in
     bit_positions' a1 (p + 1) (i lsr 1)
  in List.rev (bit_positions' [] 0 i)

(** Construct the skew binary number corresponding to an integer *)
let of_int i =
  let bp = bit_positions i in
  let l = List.length bp in
  let bp' = match bp with
  | 0 :: rest -> rest
  | _ -> bp in
  bp' |> List.map (fun i -> (1 lsl i) - 1) |> inc' l

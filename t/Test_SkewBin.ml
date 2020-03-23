module Q = QCheck
module G = QCheck.Gen
module T = QCheck.Test
module P = QCheck.Print

let ( ==> ) = Q.( ==> )
let ( >>= ) = G.( >>= )

let skew_arb_nat = Q.make G.nat ~print:P.int

let skew_test_nat ~name = T.make ~name ~long_factor:10 skew_arb_nat

let t_empty_iff_zero =
  skew_test_nat ~name:"empty_iff_zero"
  ( fun n ->
    match n |> SkewBin.of_int |> SkewBin.weights with
    | [] -> n = 0
    | _ -> n > 0
  )

let skew_pow2 n =
  0 = (n land (n + 1))

let t_skew_powers =
  skew_test_nat ~name:"skew_powers"
  ( fun n ->
    let w = n |> SkewBin.of_int |> SkewBin.weights in
    List.for_all skew_pow2 w
  )

let rec strictly_increasing = function
  | [] -> true
  | [_] -> true
  | m :: n :: _ when m >= n -> false
  | _ :: rest -> strictly_increasing rest

let almost_increasing = function
  | [] -> true
  | [_] -> true
  | m :: (n :: _ as rest) -> m <= n && strictly_increasing rest

let t_almost_increasing =
  skew_test_nat ~name:"almost_increasing"
  ( fun n ->
    let w = n |> SkewBin.of_int |> SkewBin.weights in
    almost_increasing w
  )

let t_roundtrip =
  skew_test_nat ~name:"roundtrip"
  ( fun n ->
    n = (n |> SkewBin.of_int |> SkewBin.to_int)
  )

let roundtrip_dec n = n |> SkewBin.of_int |> SkewBin.dec |> SkewBin.to_int

let t_decrement =
  skew_test_nat ~name:"decrement"
  ( fun n ->
    try roundtrip_dec n = n - 1 with
    | Invalid_argument _ -> n = 0
  )

let testlist =
  [ t_empty_iff_zero
  ; t_skew_powers
  ; t_almost_increasing
  ; t_roundtrip
  ; t_decrement
  ]

module Q = QCheck
module G = QCheck.Gen
module T = QCheck.Test
module P = QCheck.Print

let ( ==> ) = Q.( ==> )
let ( >>= ) = G.( >>= )

open Upto

let takedrop_print = P.(pair int (list int))

let takedrop_arb_invalid =
  let gn = G.small_nat in
  gn >>= (fun n -> G.(pair ((n+1) -- max_int) (list_repeat n nat))) |>
  Q.make ~print:takedrop_print

let takedrop_test_invalid ~name = T.make ~name ~long_factor:10 takedrop_arb_invalid

let t_take_invalid =
    takedrop_test_invalid ~name:"take_invalid"
    ( fun (n, s) ->
      try Listutils.take n s |> ignore ; false with
      | Invalid_argument _ -> true
    )

let t_drop_invalid =
    takedrop_test_invalid ~name:"drop_invalid"
    ( fun (n, s) ->
      try Listutils.drop n s |> ignore ; false with
      | Invalid_argument _ -> true
    )

let takedrop_arb_neg =
  let gn = G.small_nat in
  gn >>= (fun n -> G.(pair (min_int -- -1) (list_repeat n nat))) |>
  Q.make ~print:takedrop_print

let takedrop_test_neg ~name = T.make ~name ~long_factor:10 takedrop_arb_neg

let t_take_neg =
    takedrop_test_neg ~name:"take_neg"
    ( fun (n, s) ->
      try Listutils.take n s |> ignore ; false with
      | Invalid_argument _ -> true
    )

let t_drop_neg =
    takedrop_test_neg ~name:"drop_neg"
    ( fun (n, s) ->
      try Listutils.drop n s |> ignore ; false with
      | Invalid_argument _ -> true
    )

let takedrop_arb =
  let gs = G.(small_list nat) in
  gs >>= (fun s -> G.(pair (List.length s |> int_bound) (return s))) |>
  Q.make ~print:takedrop_print

let takedrop_test ~name = T.make ~name ~long_factor:10 takedrop_arb

let t_take_length =
    takedrop_test ~name:"take_length"
    ( fun (n, s) ->
      let lt = Listutils.take n s |> List.length in
      lt = n
    )

let t_take_sublist =
    takedrop_test ~name:"take_sublist"
    ( fun (n, s) ->
      let taken = Listutils.take n s in
      let lt = List.length taken in
      List.(for_all (fun i -> nth s i = nth taken i) (upto lt))
    )

let t_drop_length =
    takedrop_test ~name:"drop_length"
    ( fun (n, s) ->
      let l = List.length s in
      let ld = Listutils.drop n s |> List.length in
      ld = l - n
    )

let t_drop_sublist =
    takedrop_test ~name:"drop_sublist"
    ( fun (n, s) ->
      let l = List.length s in
      let dropped = Listutils.drop n s in
      List.(for_all (fun i -> i < n || nth s i = nth dropped (i - n)) (upto l))
    )

let t_take_drop_append =
    takedrop_test ~name:"take_drop_append"
    ( fun (n, s) ->
      let l = List.length s in
      let remade = Listutils.(take n s @ drop n s) in
      List.(for_all (fun i -> nth s i = nth remade i) (upto l))
    )

let testlist =
  [ t_take_length
  ; t_take_invalid
  ; t_take_sublist
  ; t_drop_invalid
  ; t_take_neg
  ; t_drop_neg
  ; t_drop_length
  ; t_drop_sublist
  ; t_take_drop_append
  ]

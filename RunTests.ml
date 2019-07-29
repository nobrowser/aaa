module Q = QCheck
module G = QCheck.Gen
module T = QCheck.Test
module P = QCheck.Print
module R = QCheck_base_runner

let ( ==> ) = Q.( ==> )

let takedrop_print = P.(pair int (list int))

let takedrop_arb = G.(pair small_nat (small_list small_nat)) |> Q.make ~print:takedrop_print

let takedrop_test ~name = T.make ~name ~long_factor:10 takedrop_arb

let upto n = List.init n (fun i -> i)
                  
let t_take_invalid =
    takedrop_test ~name:"take_invalid"
    ( fun (n, s) ->
      Q.assume (n > List.length s) ;
      try Aaa__Listutils.take n s |> ignore ; false with
      | Invalid_argument _ -> true
    )
                 
let t_take_length =
    takedrop_test ~name:"take_length"
    ( fun (n, s) ->
      Q.assume (n <= List.length s) ;
      let lt = Aaa__Listutils.take n s |> List.length in
      lt = n
    )

let t_take_sublist =
    takedrop_test ~name:"take_sublist"
    ( fun (n, s) ->
      Q.assume (n <= List.length s) ;
      let taken = Aaa__Listutils.take n s in
      let lt = List.length taken in
      List.(for_all (fun i -> nth s i = nth taken i) (upto lt))
    )

let t_drop_invalid =
    takedrop_test ~name:"drop_invalid"
    ( fun (n, s) ->
      Q.assume (n > List.length s) ;
      try Aaa__Listutils.drop n s |> ignore ; false with
      | Invalid_argument _ -> true
    )

let t_drop_length =
    takedrop_test ~name:"drop_length"
    ( fun (n, s) ->
      let l = List.length s in
      Q.assume (n <= l) ;
      let ld = Aaa__Listutils.drop n s |> List.length in
      ld = l - n
    )

let t_drop_sublist =
    takedrop_test ~name:"drop_sublist"
    ( fun (n, s) ->
      let l = List.length s in
      Q.assume (n <= l) ;
      let dropped = Aaa__Listutils.drop n s in
      List.(for_all (fun i -> i < n || nth s i = nth dropped (i - n)) (upto l))
    )

let t_take_drop_append =
    takedrop_test ~name:"take_drop_append"
    ( fun (n, s) ->
      let l = List.length s in
      Q.assume (n <= l) ;
      let remade = Aaa__Listutils.(take n s @ drop n s) in
      List.(for_all (fun i -> nth s i = nth remade i) (upto l))
    )

let suite =
  [ t_take_length
  ; t_take_invalid
  ; t_take_sublist
  ; t_drop_invalid
  ; t_drop_length
  ; t_drop_sublist
  ; t_take_drop_append
  ]

let _ = R.run_tests_main suite

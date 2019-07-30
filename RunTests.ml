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

let strgen =
  G.(string
     ~gen:(frequency
           [(1, oneofl [' '; '\t']);
            (7, printable)]))

let tok_all_arb = strgen |> Q.make ~print:P.string

let tok_all_test ~name = T.make ~name ~long_factor:10 tok_all_arb

let wsp = function ' ' | '\t' -> true | _ -> false

let nwsp = fun c -> not (wsp c)

let string_forall ~f:f s =
  let s' = String.map (fun c -> if f c then '+' else '-') s in
  not (String.contains s' '-')

let t_tok_all_no_rest =
    tok_all_test ~name:"tok_all_no_rest"
    ( fun s ->
      let _ , restopt = Aaa__Strutils.token_bounds ~f:wsp s in
      match restopt with None -> true | _ -> false
    )

let rec all_within_length l = function
    | [] -> true
    | (i, j) :: rest ->
       i >= 0 && i <= l && j >= 0 && j <= l && all_within_length l rest

let t_tok_all_within_length =
    tok_all_test ~name:"tok_all_within_length"
    ( fun s ->
      let l = String.length s in
      let bs, _ = Aaa__Strutils.token_bounds ~f:wsp s in
      all_within_length l bs
    )

let rec strictly_increasing l = function
    | [] -> true
    | [(i, j)] -> i < j
    | (i, j) :: (((i', j') :: _) as rest) ->
       i < j && j < i' && strictly_increasing l rest

let t_tok_all_strictly_increasing =
    tok_all_test ~name:"tok_all_strictly_increasing"
    ( fun s ->
      let l = String.length s in
      let bs, _ = Aaa__Strutils.token_bounds ~f:wsp s in
      strictly_increasing l bs
    )

let t_tok_all_transitions =
    tok_all_test ~name:"tok_all_transitions"
    ( fun s ->
      let l = String.length s in
      let bs, _ = Aaa__Strutils.token_bounds ~f:wsp s in
      List.for_all
      ( fun (i, j) ->
        ((i = 0 || wsp s.[i - 1]) &&
         (not (wsp s.[i])) &&
         (not (wsp s.[j - 1])) &&
         (j = l || wsp s.[j]))
      )  bs
    )

let t_tok_all_nomissed =
    tok_all_test ~name:"tok_all_nomissed"
    ( fun s ->
      let bs, _ = Aaa__Strutils.token_bounds ~f:wsp s in
      List.for_all
      ( fun (i, j) ->
        let ss = String.sub s i (j - i) in
        string_forall nwsp ss
      ) bs
    )

let t_tok_all_nonempty =
    tok_all_test ~name:"tok_all_nonempty"
    ( fun s ->
      let bs, _ = Aaa__Strutils.token_bounds ~f:wsp s in
      not (string_forall wsp s) ==>
      match bs with [] -> false | _ -> true
    )

let t_tok_all_empty =
    tok_all_test ~name:"tok_all_empty"
    ( fun s ->
      let bs, _ = Aaa__Strutils.token_bounds ~f:wsp s in
      Q.assume (string_forall wsp s) ;
      match bs with [] -> true | _ -> false
    )

let t_fld_all_no_rest =
    tok_all_test ~name:"fld_all_no_rest"
    ( fun s ->
      let _ , restopt = Aaa__Strutils.field_bounds ~f:wsp s in
      match restopt with None -> true | _ -> false
    )

let t_fld_all_within_length =
    tok_all_test ~name:"fld_all_within_length"
    ( fun s ->
      let l = String.length s in
      let bs, _ = Aaa__Strutils.field_bounds ~f:wsp s in
      all_within_length l bs
    )

let rec loosely_increasing l = function
    | [] -> true
    | [(i, j)] -> i <= j
    | (i, j) :: (((i', j') :: _) as rest) ->
       i <= j && j < i' && loosely_increasing l rest

let t_fld_all_loosely_increasing =
    tok_all_test ~name:"fld_all_loosely_increasing"
    ( fun s ->
      let l = String.length s in
      let bs, _ = Aaa__Strutils.field_bounds ~f:wsp s in
      loosely_increasing l bs
    )

let t_fld_all_nonempty =
    tok_all_test ~name:"fld_all_nonempty"
    ( fun s ->
      let bs, _ = Aaa__Strutils.field_bounds ~f:wsp s in
      match bs with [] -> false | _ -> true
    )

let t_fld_all_rear_aligned =
    tok_all_test ~name:"fld_all_rear_aligned"
    ( fun s ->
      let l = String.length s in
      let bs, _ = Aaa__Strutils.field_bounds ~f:wsp s in
      List.exists (fun (_, j) -> j = l) bs
    )

let t_fld_all_front_aligned =
    tok_all_test ~name:"fld_all_front_aligned"
    ( fun s ->
      let bs, _ = Aaa__Strutils.field_bounds ~f:wsp s in
      List.exists (fun (i, _) -> i = 0) bs
    )

let t_fld_all_transitions =
    tok_all_test ~name:"fld_all_transitions"
    ( fun s ->
        let l = String.length s in
        let bs, _ = Aaa__Strutils.field_bounds ~f:wsp s in
        List.for_all
        ( fun (i, j) ->
          (i = 0 || wsp s.[i - 1]) &&
          (j = l || wsp s.[j]) &&
          (i = j || (nwsp s.[i] && nwsp s.[j - 1]))
        )  bs
      )

let t_fld_all_nomissed =
    tok_all_test ~name:"fld_all_nomissed"
    ( fun s ->
      let bs, _ = Aaa__Strutils.field_bounds ~f:wsp s in
      List.for_all
      ( fun (i, j) ->
        (i = j) ||
        let ss = String.sub s i (j - i) in
        string_forall nwsp ss
      ) bs
    )

let suite =
  [ t_take_length
  ; t_take_invalid
  ; t_take_sublist
  ; t_drop_invalid
  ; t_drop_length
  ; t_drop_sublist
  ; t_take_drop_append
  ; t_tok_all_no_rest
  ; t_tok_all_within_length
  ; t_tok_all_strictly_increasing
  ; t_tok_all_transitions
  ; t_tok_all_nomissed
  ; t_tok_all_nonempty
  ; t_tok_all_empty
  ; t_fld_all_no_rest
  ; t_fld_all_within_length
  ; t_fld_all_loosely_increasing
  ; t_fld_all_nonempty
  ; t_fld_all_rear_aligned
  ; t_fld_all_front_aligned
  ; t_fld_all_transitions
  ; t_fld_all_nomissed
  ]

let _ = R.run_tests_main suite

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
      try Listutils.take n s |> ignore ; false with
      | Invalid_argument _ -> true
    )

let t_take_length =
    takedrop_test ~name:"take_length"
    ( fun (n, s) ->
      Q.assume (n <= List.length s) ;
      let lt = Listutils.take n s |> List.length in
      lt = n
    )

let t_take_sublist =
    takedrop_test ~name:"take_sublist"
    ( fun (n, s) ->
      Q.assume (n <= List.length s) ;
      let taken = Listutils.take n s in
      let lt = List.length taken in
      List.(for_all (fun i -> nth s i = nth taken i) (upto lt))
    )

let t_drop_invalid =
    takedrop_test ~name:"drop_invalid"
    ( fun (n, s) ->
      Q.assume (n > List.length s) ;
      try Listutils.drop n s |> ignore ; false with
      | Invalid_argument _ -> true
    )

let t_drop_length =
    takedrop_test ~name:"drop_length"
    ( fun (n, s) ->
      let l = List.length s in
      Q.assume (n <= l) ;
      let ld = Listutils.drop n s |> List.length in
      ld = l - n
    )

let t_drop_sublist =
    takedrop_test ~name:"drop_sublist"
    ( fun (n, s) ->
      let l = List.length s in
      Q.assume (n <= l) ;
      let dropped = Listutils.drop n s in
      List.(for_all (fun i -> i < n || nth s i = nth dropped (i - n)) (upto l))
    )

let t_take_drop_append =
    takedrop_test ~name:"take_drop_append"
    ( fun (n, s) ->
      let l = List.length s in
      Q.assume (n <= l) ;
      let remade = Listutils.(take n s @ drop n s) in
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
      let _ , restopt = Strutils.token_bounds ~f:wsp s in
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
      let bs, _ = Strutils.token_bounds ~f:wsp s in
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
      let bs, _ = Strutils.token_bounds ~f:wsp s in
      strictly_increasing l bs
    )

let t_tok_all_transitions =
    tok_all_test ~name:"tok_all_transitions"
    ( fun s ->
      let l = String.length s in
      let bs, _ = Strutils.token_bounds ~f:wsp s in
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
      let bs, _ = Strutils.token_bounds ~f:wsp s in
      List.for_all
      ( fun (i, j) ->
        let ss = String.sub s i (j - i) in
        string_forall nwsp ss
      ) bs
    )

let t_tok_all_nonempty =
    tok_all_test ~name:"tok_all_nonempty"
    ( fun s ->
      let bs, _ = Strutils.token_bounds ~f:wsp s in
      not (string_forall wsp s) ==>
      match bs with [] -> false | _ -> true
    )

let t_tok_all_empty =
    tok_all_test ~name:"tok_all_empty"
    ( fun s ->
      let bs, _ = Strutils.token_bounds ~f:wsp s in
      Q.assume (string_forall wsp s) ;
      match bs with [] -> true | _ -> false
    )

let t_fld_all_no_rest =
    tok_all_test ~name:"fld_all_no_rest"
    ( fun s ->
      let _ , restopt = Strutils.field_bounds ~f:wsp s in
      match restopt with None -> true | _ -> false
    )

let t_fld_all_within_length =
    tok_all_test ~name:"fld_all_within_length"
    ( fun s ->
      let l = String.length s in
      let bs, _ = Strutils.field_bounds ~f:wsp s in
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
      let bs, _ = Strutils.field_bounds ~f:wsp s in
      loosely_increasing l bs
    )

let t_fld_all_nonempty =
    tok_all_test ~name:"fld_all_nonempty"
    ( fun s ->
      let bs, _ = Strutils.field_bounds ~f:wsp s in
      match bs with [] -> false | _ -> true
    )

let t_fld_all_rear_aligned =
    tok_all_test ~name:"fld_all_rear_aligned"
    ( fun s ->
      let l = String.length s in
      let bs, _ = Strutils.field_bounds ~f:wsp s in
      List.exists (fun (_, j) -> j = l) bs
    )

let t_fld_all_front_aligned =
    tok_all_test ~name:"fld_all_front_aligned"
    ( fun s ->
      let bs, _ = Strutils.field_bounds ~f:wsp s in
      List.exists (fun (i, _) -> i = 0) bs
    )

let t_fld_all_transitions =
    tok_all_test ~name:"fld_all_transitions"
    ( fun s ->
        let l = String.length s in
        let bs, _ = Strutils.field_bounds ~f:wsp s in
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
      let bs, _ = Strutils.field_bounds ~f:wsp s in
      List.for_all
      ( fun (i, j) ->
        (i = j) ||
        let ss = String.sub s i (j - i) in
        string_forall nwsp ss
      ) bs
    )

let t_fld_all_notokens =
    tok_all_test ~name:"fld_all_notokens"
    ( fun s ->
      let l = String.length s in
      let bs, _ = Strutils.field_bounds ~f:wsp s in
      Q.assume (string_forall wsp s) ;
      List.(for_all (fun i -> let (j, k) = nth bs i in j = i && k = i) (upto l))
    )

let tok_max_print = P.(pair int string)

let tok_max_arb = G.(pair small_nat strgen) |> Q.make ~print:tok_max_print

let tok_max_test ~name = T.make ~name ~long_factor:10 tok_max_arb

let t_tok_max_short =
    tok_max_test ~name:"tok_max_short"
    ( fun (n, s) ->
      let bs, _ = Strutils.token_bounds ~f:wsp ~max:n s in
      let l = List.length bs in
      l <= n
    )

let t_tok_max_norest =
    tok_max_test ~name:"tok_max_norest"
    ( fun (n, s) ->
      let bs, rest = Strutils.token_bounds ~f:wsp ~max:n s in
      let l = List.length bs in
      match rest with
      | None -> Q.assume_fail ()
      | _ -> l = n
    )

let t_tok_max_rear_aligned =
    tok_max_test ~name:"tok_max_rear_aligned"
    ( fun (n, s) ->
      let _, rest = Strutils.token_bounds ~f:wsp ~max:n s in
      let l = String.length s in
      match rest with
      | None -> Q.assume_fail ()
      | Some (_, k) -> k = l
    )

let t_tok_max_rest_rightmost =
    tok_max_test ~name:"tok_max_rest_rightmost"
    ( fun (n, s) ->
      let bs, rest = Strutils.token_bounds ~f:wsp ~max:n s in
      match rest with
      | None -> Q.assume_fail ()
      | Some (k, _) -> List.for_all (fun (_, j) -> j < k) bs
    )

let t_fld_max_short =
    tok_max_test ~name:"fld_max_short"
    ( fun (n, s) ->
      let bs, _ = Strutils.field_bounds ~f:wsp ~max:n s in
      let l = List.length bs in
      l <= n
    )

let t_fld_max_norest =
    tok_max_test ~name:"fld_max_norest"
    ( fun (n, s) ->
      let bs, rest = Strutils.field_bounds ~f:wsp ~max:n s in
      let l = List.length bs in
      match rest with
      | None -> Q.assume_fail ()
      | _ -> l = n
    )

let t_fld_max_rear_aligned =
    tok_max_test ~name:"fld_max_rear_aligned"
    ( fun (n, s) ->
      let _, rest = Strutils.field_bounds ~f:wsp ~max:n s in
      let l = String.length s in
      match rest with
      | None -> Q.assume_fail ()
      | Some (_, k) -> k = l
    )

let t_fld_max_rest_rightmost =
    tok_max_test ~name:"fld_max_rest_rightmost"
    ( fun (n, s) ->
      let bs, rest = Strutils.field_bounds ~f:wsp ~max:n s in
      match rest with
      | None -> Q.assume_fail ()
      | Some (k, _) -> List.for_all (fun (_, j) -> j <= k) bs
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
  ; t_fld_all_notokens
  ; t_tok_max_short
  ; t_tok_max_norest
  ; t_tok_max_rear_aligned
  ; t_tok_max_rest_rightmost
  ; t_fld_max_short
  ; t_fld_max_norest
  ; t_fld_max_rear_aligned
  ; t_fld_max_rest_rightmost
  ]

let _ = R.run_tests_main suite

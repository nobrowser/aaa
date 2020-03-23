module Q = QCheck
module G = QCheck.Gen
module T = QCheck.Test
module P = QCheck.Print

let ( ==> ) = Q.( ==> )
let ( >>= ) = G.( >>= )

let charlist = List.map Char.chr (upto 255)

let chargen p = List.filter p charlist |> G.oneofl

let charpr c = Printf.sprintf "\\%.2x" (Char.code c)

let wsp = function ' ' | '\t' -> true | _ -> false

let nwsp = fun c -> not (wsp c)

let strpr s =
  String.to_seq s |>
  List.of_seq |>
  List.map charpr |>
  String.concat "" |>
  Printf.sprintf "\"%s\""

let strgen_normal =
  G.(small_string
     ~gen:(frequency
           [(1, chargen wsp);
            (7, chargen nwsp)]))

let strgen_allwsp = G.small_string ~gen:(chargen wsp)

let strgen_allnwsp = G.small_string ~gen:(chargen nwsp)

let tok_all_arb = strgen_normal |> Q.make ~print:strpr

let tok_all_test ~name = T.make ~name ~long_factor:10 tok_all_arb

let tok_all_wsp_arb = strgen_allwsp |> Q.make ~print:strpr

let tok_all_wsp_test ~name = T.make ~name ~long_factor:10 tok_all_wsp_arb

let tok_all_nwsp_arb = strgen_allnwsp |> Q.make ~print:strpr

let tok_all_nwsp_test ~name = T.make ~name ~long_factor:10 tok_all_nwsp_arb

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
    tok_all_nwsp_test ~name:"tok_all_nonempty"
    ( fun s ->
      String.length s = 0 ||
      let bs, _ = Strutils.token_bounds ~f:wsp s in
      match bs with [] -> false | _ -> true
    )

let t_tok_all_empty =
    tok_all_wsp_test ~name:"tok_all_empty"
    ( fun s ->
      let bs, _ = Strutils.token_bounds ~f:wsp s in
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
    tok_all_wsp_test ~name:"fld_all_notokens"
    ( fun s ->
      let l = String.length s in
      let bs, _ = Strutils.field_bounds ~f:wsp s in
      List.(for_all (fun i -> let (j, k) = nth bs i in j = i && k = i) (upto l))
    )

let tok_max_print = P.(pair int string)

let tok_max_arb = G.(pair small_nat strgen_normal) |> Q.make ~print:tok_max_print

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
      | None -> true
      | _ -> l = n
    )

let t_tok_max_rear_aligned =
    tok_max_test ~name:"tok_max_rear_aligned"
    ( fun (n, s) ->
      let _, rest = Strutils.token_bounds ~f:wsp ~max:n s in
      let l = String.length s in
      match rest with
      | None -> true
      | Some (_, k) -> k = l
    )

let t_tok_max_rest_rightmost =
    tok_max_test ~name:"tok_max_rest_rightmost"
    ( fun (n, s) ->
      let bs, rest = Strutils.token_bounds ~f:wsp ~max:n s in
      match rest with
      | None -> true
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
      | None -> true
      | _ -> l = n
    )

let t_fld_max_rear_aligned =
    tok_max_test ~name:"fld_max_rear_aligned"
    ( fun (n, s) ->
      let _, rest = Strutils.field_bounds ~f:wsp ~max:n s in
      let l = String.length s in
      match rest with
      | None -> true
      | Some (_, k) -> k = l
    )

let t_fld_max_rest_rightmost =
    tok_max_test ~name:"fld_max_rest_rightmost"
    ( fun (n, s) ->
      let bs, rest = Strutils.field_bounds ~f:wsp ~max:n s in
      match rest with
      | None -> true
      | Some (k, _) -> List.for_all (fun (_, j) -> j <= k) bs
    )

let testlist =
  [ t_tok_all_no_rest
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

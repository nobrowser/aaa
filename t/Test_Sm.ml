module Q = QCheck
module G = QCheck.Gen
module T = QCheck.Test
module P = QCheck.Print

let ( ==> ) = Q.( ==> )
let ( >>= ) = G.( >>= )

let alist_print = P.(list string)

let alist_dupe_arb = G.((list_size (1 -- 100) small_string) >>= function
                        | [] -> assert false
                        | s :: ss -> shuffle_l (s :: s :: ss))
                     |> Q.make ~print:alist_print

let alist_dupe_test ~name = T.make ~name ~long_factor:10 alist_dupe_arb

let t_alist_dupe_errs =
    alist_dupe_test ~name:"alist_dupe_errs"
    ( fun l ->
      let l' = List.map (fun s -> (s, ())) l in
      match Sm.of_list_no_repeats l' with
      | Error _ -> true
      | _ -> false
    )

let alist_nodupe_arb = G.((list_size (1 -- 100) small_string) >>= fun l ->
                          shuffle_l (List.sort_uniq String.compare l))
                     |> Q.make ~print:alist_print

let alist_nodupe_test ~name = T.make ~name ~long_factor:10 alist_nodupe_arb

let t_alist_nodupe_ok =
    alist_nodupe_test ~name:"alist_nodupe_ok"
    ( fun l ->
      let l' = List.map (fun s -> (s, ())) l in
      match Sm.of_list_no_repeats l' with
      | Ok _ -> true
      | _ -> false
    )

let testlist =
  [ t_alist_dupe_errs
  ; t_alist_nodupe_ok
  ]

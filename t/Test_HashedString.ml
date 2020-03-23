module Q = QCheck
module G = QCheck.Gen
module T = QCheck.Test
module P = QCheck.Print

let ( ==> ) = Q.( ==> )
let ( >>= ) = G.( >>= )

let hashed_arb = strgen_normal |> Q.make ~print:strpr

let hashed_test ~name = T.make ~name ~long_factor:10 hashed_arb

let t_hashed_copy =
    hashed_test ~name:"hashed_copy"
    ( fun s ->
      let s' = String.(sub s 0 (length s)) in
      HashedString.(hash s = hash s')
    )

let rot13 c =
    let o = Char.code c in
    (o + 13) mod 256 |> Char.chr

let t_hashed_splits_close =
    hashed_test ~name:"hashed_splits_close"
    ( fun s ->
      let f i =
        String.mapi (fun j c -> if i = j then rot13 c else c) s in
      let buddies = s :: List.map f (upto (String.length s)) in
      let hashes = List.map HashedString.hash buddies in
      let tbl = Hashtbl.create 300 in
      try
      ( for i = 0 to (List.length hashes - 1) do
        let h =  List.nth hashes i in
        if Hashtbl.mem tbl h then raise Exit
        else Hashtbl.add tbl h ()
        done ; true
      ) with Exit -> false
    )

let testlist =
  [ t_hashed_copy
  ; t_hashed_splits_close
  ]

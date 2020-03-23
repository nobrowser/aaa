module Q = QCheck
module G = QCheck.Gen
module T = QCheck.Test
module P = QCheck.Print

let ( ==> ) = Q.( ==> )
let ( >>= ) = G.( >>= )

open Upto

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

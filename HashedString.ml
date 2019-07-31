include String

let hash s =
  let sum24 a c = (Char.code c + (a lsr 1) lor ((a land 1) lsl 23)) land 0xffffff in
  let fold_left' = Seq.fold_left sum24 0 in
  s |> to_seq |> fold_left'

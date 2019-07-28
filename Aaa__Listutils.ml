let take n l =
  if n < 0 then invalid_arg "take" else
  let rec take' ac n' l' =
    if n' = 0 then ac
    else match l' with
    | x :: xs -> take' (x :: ac) (n' - 1) xs
    | [] -> invalid_arg "take" in
  take' [] n l |> List.rev

let rec drop n l =
  if n < 0 then invalid_arg "drop"
  else if n = 0 then l
  else match l with
       | [] -> invalid_arg "drop"
       | x :: xs -> drop (n - 1) xs

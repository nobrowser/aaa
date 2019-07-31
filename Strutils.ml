type tokstate = In | Out

let substrings_of_bounds s (bs, tailopt) =
    let base = List.map (fun (i, j) -> String.sub s i (j - i)) bs in
    match tailopt with
    | None -> (base, None)
    | Some (i, j) -> (base, Some (String.sub s i (j - i)))

let trim_bounds maxopt l bs =
    let n = List.length bs in
    match maxopt with
    | None -> (bs, None)
    | Some m when m >= n -> (bs, None)
    | Some m when m >= 0 ->
       let head, tail = Listutils.(take m bs, drop m bs) in
       let (i, _) = List.hd tail in
       (head, Some (i, l))
    | _ -> invalid_arg "Strutils.trim_bounds"

let replace_front k = function
    | [] -> assert false
    | (_, j) :: rest -> (k, j) :: rest

let token_bounds ?max ~f:f s =
    let l = String.length s in
    let rec bounds ac st = function
      | 0 -> ac
      | i ->
         if f s.[i - 1] then bounds ac Out (i - 1) else
         ( match st with
           | Out -> bounds ((i - 1, i) :: ac) In (i - 1)
           | In -> bounds (replace_front (i - 1) ac) In (i - 1)
         ) in
    bounds [] Out l |> trim_bounds max l

let tokens ?max ~f s = token_bounds ?max ~f s |> substrings_of_bounds s

let field_bounds ?max ~f:f s =
    let l = String.length s in
    let rec bounds ac st = function
      | 0 ->
         ( match st with
           | In -> replace_front 0 ac
           | Out -> ((0, 0) :: ac)
         )
      | i ->
         ( match st with
           | In ->
              if f s.[i - 1] then bounds ac Out (i - 1)
              else bounds (replace_front (i - 1) ac) In (i - 1)
           | Out ->
              if f s.[i - 1] then bounds ((i, i) :: ac) Out (i - 1)
              else bounds ((i - 1, i) :: ac) In (i - 1)
         ) in
    bounds [(l, l)] In l |> trim_bounds max l

let fields ?max ~f s = field_bounds ?max ~f s |> substrings_of_bounds s

let is_prefix p s =
    let lp, ls = String.(length p, length s) in
    lp <= ls && String.(equal p (sub s 0 lp))

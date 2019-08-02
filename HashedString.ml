include String

let hash s =
    let d = Digest.(string s |> to_hex) in
    let l = if Sys.int_size <= 32 then 7 else 15 in
    int_of_string ("0x" ^ String.sub d 13 l)

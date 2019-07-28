module StringMap = Map.Make(String)

let of_list_no_repeats l =
  let rec add1 m (k, v) =
    if StringMap.mem k m then "repeated key " ^ k |> Resultx.error
    else Resultx.ok (StringMap.add k v m) in
  Resultx.lfold add1 StringMap.empty l

module StringMapRx = Resultx.Maps (StringMap)

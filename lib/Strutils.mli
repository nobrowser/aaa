val tokens : ?max:int -> f:(char -> bool) -> string ->
             (string list * string option)

val fields : ?max:int -> f:(char -> bool) -> string ->
             (string list * string option)

val token_bounds : ?max:int -> f:(char -> bool) -> string ->
                   ((int * int) list * (int * int) option)

val field_bounds : ?max:int -> f:(char -> bool) -> string ->
                   ((int * int) list * (int * int) option)

val is_prefix : string -> string -> bool

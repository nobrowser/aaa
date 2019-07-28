val with_file : string -> f:(in_channel -> 'a) -> 'a

val fold_lines : f:('a -> string -> 'a) -> init:'a -> in_channel -> 'a

val iter_lines : f:(string -> unit) -> in_channel -> unit

val fold_file_lines : f:('a -> string -> 'a) -> init:'a -> string -> 'a

val iter_file_lines : f:(string -> unit) -> string -> unit

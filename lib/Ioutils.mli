val with_in_file : string -> f:(in_channel -> 'a) -> 'a

val lines_of_in_chan : in_channel -> string Seq.t

val fold_lines : f:('a -> string -> 'a) -> init:'a -> in_channel -> 'a

val iter_lines : f:(string -> unit) -> in_channel -> unit

val fold_file_lines : f:('a -> string -> 'a) -> init:'a -> string -> 'a

val iter_file_lines : f:(string -> unit) -> string -> unit

val chars_of_in_chan : in_channel -> char Seq.t

val fold_chars : f:('a -> char -> 'a) -> init:'a -> in_channel -> 'a

val iter_chars : f:(char -> unit) -> in_channel -> unit

val fold_file_chars : f:('a -> char -> 'a) -> init:'a -> string -> 'a

val iter_file_chars : f:(char -> unit) -> string -> unit

val seq_of_in_chan :
    in_channel ->
    fmt:(('a, Scanf.Scanning.in_channel, 'b, 'c -> 'd, 'a -> 'e, 'e) format6) ->
    r:'c ->
    'd Seq.t

val with_in_file_seq :
    m:('d Seq.t -> 'f) ->
    fn:string ->
    fmt:(('a, Scanf.Scanning.in_channel, 'b, 'c -> 'd, 'a -> 'e, 'e) format6) ->
    r:'c ->
    'f

val with_out_file : string -> f:(out_channel -> 'a) -> 'a

let with_in_file fn ~f:reader =
    let ch = open_in fn in
    Exnutils.protect ~finally:(fun () -> close_in ch) ~f:(fun () -> reader ch)

let lines_of_in_chan inch =
    let rec delay () =
      Seq.(try Cons (input_line inch, delay) with End_of_file -> Nil)
    in delay

let fold_lines ~f ~init inch =
    Seq.fold_left f init (lines_of_in_chan inch)

let iter_lines ~f inch =
    Seq.iter f (lines_of_in_chan inch)

let fold_file_lines ~f:lf ~init fn = with_in_file fn ~f:(fold_lines ~f:lf ~init)

let iter_file_lines ~f:lf fn = with_in_file fn ~f:(iter_lines ~f:lf)

let chars_of_in_chan inch =
    let rec delay () =
      Seq.(try Cons (input_char inch, delay) with End_of_file -> Nil)
    in delay

let fold_chars ~f ~init inch =
    Seq.fold_left f init (chars_of_in_chan inch)

let iter_chars ~f inch =
    Seq.iter f (chars_of_in_chan inch)

let fold_file_chars ~f:lf ~init fn = with_in_file fn ~f:(fold_chars ~f:lf ~init)

let iter_file_chars ~f:lf fn = with_in_file fn ~f:(iter_chars ~f:lf)

let seq_of_in_chan inch ~fmt ~r =
    let scanner = Scanf.(bscanf (Scanning.from_channel inch) fmt) in
    let rec delay () = Seq.(try Cons (scanner r, delay) with End_of_file -> Nil)
    in delay

let with_in_file_seq ~m:m ~fn  ~fmt ~r =
    with_in_file fn
    ~f:(fun inch -> seq_of_in_chan inch ~fmt ~r |> m)

let with_out_file fn ~f:writer =
    let ch = open_out fn in
    Exnutils.protect ~finally:(fun () -> close_out ch) ~f:(fun () -> writer ch)

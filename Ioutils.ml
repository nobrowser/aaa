let with_file fn ~f:reader =
  let ch = open_in fn in
  Exnutils.protect ~finally:(fun () -> close_in ch) ~f:(fun () -> reader ch)

let seq_of_in_chan inch =
  let rec delay () =
    try Seq.Cons (input_line inch, delay) with
    | End_of_file -> Seq.Nil
  in delay

let fold_lines ~f ~init inch =
  Seq.fold_left f init (seq_of_in_chan inch)

let iter_lines ~f inch =
  Seq.iter f (seq_of_in_chan inch)

let fold_file_lines ~f:lf ~init fn = with_file fn ~f:(fold_lines ~f:lf ~init)

let iter_file_lines ~f:lf fn = with_file fn ~f:(iter_lines ~f:lf)

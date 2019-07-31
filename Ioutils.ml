let with_file fn ~f:reader =
  let ch = open_in fn in
  Exnutils.protect ~finally:(fun () -> close_in ch) ~f:(fun () -> reader ch)

let fold_lines ~f:f ~init inch =
  let res = ref init in
  try while true do res := f !res (input_line inch) done ; assert false with
  | End_of_file -> !res

let iter_lines ~f:f inch = fold_lines ~f:(fun _ s -> f s) ~init:() inch

let fold_file_lines ~f:lf ~init fn = with_file fn ~f:(fold_lines ~f:lf ~init)

let iter_file_lines ~f:lf fn = with_file fn ~f:(iter_lines ~f:lf)

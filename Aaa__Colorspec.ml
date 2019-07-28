open Resultx.Monad
open Sm

module type CONTEXTS =
  sig

  include Map.OrderedType

  val read : string -> (t, string) Resultx.t
  val pp : Format.formatter -> t -> unit
  val show : t -> string

  end

let int_res_of_string s =
  try int_of_string s |> return with Failure e -> Resultx.error e

let wsp = function ' ' | '\t' -> true | _ -> false

type rgb = {name: string; r: int; g: int; b: int}

let parse_rgb_line s =
  match Strutils.tokens ~max:3 ~f:wsp s with
  | [tr; tg; tb], Some rest ->
     let name = String.trim rest in
     int_res_of_string tr >>= fun r ->
     int_res_of_string tg >>= fun g ->
     int_res_of_string tb >>| fun b ->
     {name; r; g; b}
  | _ -> Resultx.error "invalid rgb line"

let parse_color ~rgbmap s =
  if Strutils.is_prefix "0x" s || Strutils.is_prefix "0X" s then
  ( int_res_of_string s
  ) else if Strutils.is_prefix "#" s then
  ( let l = String.length s in
    let s' = String.sub s 1 (l - 1) in
    int_res_of_string ("0x" ^ s')
  ) else if Strutils.is_prefix "X/" s then
  ( let l = String.length s in
    let s' = String.sub s 2 (l - 2) in
    match StringMap.find_opt s' rgbmap with
    | Some c -> return c
    | None -> s' ^ ": color name not in map" |> Resultx.error
  ) else Resultx.error "parse_color: not a valid color spec"

let parse_spec ~rgbmap reader s =
  match String.split_on_char ':' s with
  | [name; color] ->
     reader name >>= fun ctx ->
     parse_color ~rgbmap color >>| fun cspec ->
     (ctx, cspec)
  | _ -> Resultx.error "parse_spec: not a valid color spec"

let addc rm {name; r; g; b} =
  rm >>| fun m ->
  let c = (r lsl 16) lor (g lsl 8) lor b in
  StringMap.add name c m

let make_rgbmap fname =
  try
  ( Ioutils.fold_file_lines
    ~f:(fun rm s -> parse_rgb_line s >>= addc rm)
    ~init:(return StringMap.empty)
    fname
  ) with Sys_error e -> Resultx.error e

let parse_specs ~rgbfile reader s =
  make_rgbmap rgbfile >>= fun rgbmap ->
  let spec_list = String.split_on_char ',' s in
    match spec_list with
    | [""] -> return []
    | _ -> List.map (parse_spec ~rgbmap reader) spec_list |> Resultx.ljoin

module type COLOR_PARSER =
  sig

  type ctx
  module CtxMap : Map.S with type key = ctx
  val parse : rgbfile:string -> string -> (int CtxMap.t, string) Resultx.t
  val cmdliner_parse : rgbfile:string -> string ->
                       (int CtxMap.t, [`Msg of string]) Resultx.t
  val cmdliner_print : Format.formatter -> int CtxMap.t -> unit

  end

module Color_parser (Contexts: CONTEXTS) : COLOR_PARSER with type ctx = Contexts.t =
  struct

  type ctx = Contexts.t
  module CtxMap = Map.Make(Contexts)

  let map_from_pairs ps =
    ps |> List.to_seq |> CtxMap.of_seq |> return

  let parse ~rgbfile specs =
    parse_specs ~rgbfile Contexts.read specs >>= map_from_pairs

  let cmdliner_parse ~rgbfile specs =
    let r = parse ~rgbfile specs in
    Resultx.fold r ~ok:(fun v -> return v) ~error:(fun m -> Resultx.error (`Msg m))

  let cmdliner_print fmt m =
    let show_item (ctx, color) =
      Printf.sprintf "%s:0x%6.6x" (Contexts.show ctx) color in
    let items =
      CtxMap.to_seq m |>
      List.of_seq |>
      List.map show_item in
    match items with
    | [] -> Format.pp_print_string fmt "built-in"
    | _ ->
       let pp_item i s =
         if i <> 0 then (Format.pp_print_string fmt "," ; Format.pp_print_cut fmt ())
         else () ;
         Format.pp_print_string fmt s in
       List.iteri pp_item items

  end

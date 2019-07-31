module type CONTEXTS =
  sig

  include Map.OrderedType

  val read : string -> (t, string) Resultx.t
  val pp : Format.formatter -> t -> unit
  val show : t -> string

  end

module type COLOR_PARSER =
  sig

  type ctx
  module CtxMap : Map.S with type key = ctx
  val parse : rgbfile:string -> string -> (int CtxMap.t, string) Resultx.t
  val cmdliner_parse : rgbfile:string -> string ->
                       (int CtxMap.t, [`Msg of string]) Resultx.t
  val cmdliner_print : Format.formatter -> int CtxMap.t -> unit

  end

module Color_parser (Contexts: CONTEXTS) : COLOR_PARSER with type ctx = Contexts.t

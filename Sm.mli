module StringMap : Map.S with type key = string

val of_list_no_repeats : (string * 'a) list ->
                         ('a StringMap.t, string) Resultx.t

module StringMapRx : Resultx.MAPS
       with module M = StringMap

(** Skew binary numbers as per Okasaki *)

(** Type of skew binary numbers *)
type t

(** List of the representation weights. *)
val weights : t -> int list

(** Get the integer value of a skew binary number *)
val to_int : t -> int

(** Construct the skew binary number corresponding to an integer *)
val of_int : int -> t

(** Increment a skew binary number *)
val inc : t -> t

(** Decrement a skew binary number *)
val dec : t -> t

(** Represents a row within a table. *)

(** The type of a row. *)
type t

(*todo*)
exception InvalidColumn of string
exception ValueMismatch of int

(** [empty] is the empty row. *)
val empty : t 

(** [value r c] is the value in [r] at column [c]. *)
val value : t -> string -> string

(** [add_column r c v] will add a column named [c] to row [r] with value [v]. *)
val add_column : t -> string -> string -> t

(** [delete_column r c] is the row [r] without column [c]. *)
val delete_column : t -> string -> t

(** [condition r c cd] is  the row [r] with only columns [c] or None if
    the conditions [cd] are not met. *)
val condition : t -> string list -> Command.conditions -> t option

(** [add_row cs vs] will create a new row with values [vs] associated with
  * the columns [cs] respectively. 
  * Requires: length of [vs] and [cs] are equivalent. *)
val build_row : string  list -> string list -> t

(** [update r c v] will update the value in row [r] at column [c] to [v]. *)
val update : t -> string -> string -> t

(** [to_csv r] converts row [r] to a csv string consisting of its values. *)
val to_csv : t -> string
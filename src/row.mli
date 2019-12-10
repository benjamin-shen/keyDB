(** 
   Row functions for the DBMS.
*)

(** Type [t] represents a row. *)
type t

(** Raised when column doesn't exist. *)
exception InvalidColumn of string

(** Raised when a row's values doesn't match its columns. *)
exception ValueMismatch of int

(** [empty] is the empty row. *)
val empty : t 

(** [value r c] is the value in [r] at column [c]. 

    Raises: [Not_found] if [c] is not associated with a value. *)
val value : t -> Command.column -> Command.value

(** [add_column r c v] will add a column named [c] to row [r] with value [v]. *)
val add_column : t -> Command.column -> Command.value -> t

(** [delete_column r c] is the row [r] without column [c]. *)
val delete_column : t -> Command.column -> t

(** [update r c v] will update the value in the row [r] at column [c] to 
    value [v]. *)
val update : t -> Command.column -> Command.value -> t

(** [condition r c cd] is Some row [r] with only columns [c] that satisfied
    conditions [cd] or None if the conditions are not met. 

    Raises: [InvalidColumn col] if [c] or a column referred in [cd] 
    doesn't exist. *)
val condition : t -> Command.column list -> Command.condition list -> t option

(** [add_row cs vs] will create a new row with values [vs] associated with
    the columns [cs] respectively.

    Raises: [ValueMismatch len] if the length [len] of [cs] and [vs] 
    are not equal. *)
val build_row : string  list -> string list -> t

(** [to_csv r] converts row [r] to a csv string consisting of its values. *)
val to_csv : t -> string
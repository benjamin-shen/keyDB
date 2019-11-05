(** Represents a table. *)


(** A [Table] containing rows and columns. *)
type t

(** [empty] is the empty table. *)
val empty : t

(** [insert_row t k r] adds a row [r] to the end of a table [t] and returns a
    table with the added row. *)
val insert_row : t -> int -> Row.t -> t

(** [remove_row t k] removes a row with key [k] from a table [t] and returns a
    table without the specified row. *)
val remove_row : t -> int -> t

(** [get_column t c] finds a column [col] in a table [t] and 
    returns the contents of that column. *)
val get_column : t -> string -> string list

(** [select t c] finds the rows in [t] that satisfy the conditions [c] and
    returns a table containing these rows. 
    Raises: some error. *)
val select : t -> string list -> t

(** [add_column t c] adds a column [col] to the end of a table [t] and returns
    a table [t'] with the added column. *)
val add_column : t -> string -> t

(** [remove_column t c] removes a column [col] from a table [t] and returns 
    a table without the specified column. 
    Raises: some error. *)
val remove_column : t -> string -> t

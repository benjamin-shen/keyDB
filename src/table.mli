(** Represents a table. *)


(** A [Table] containing rows and columns. *)
type t

(** [insert_row t r] adds a row [r] to the end of a table [t] and returns a
    table [t'] with the added row. *)
val insert_row : t -> Row.t -> t

(** [remove_row t r] removes a row [r] from a table [t] and returns a
    table without the specified row. *)
val remove_row : t -> Row.t -> t

(** [select t c] finds the rows in [t] that satisfy the conditions [c] and
    returns a table containing these rows. 
    Raises: some error. *)
val select : t -> string list -> t

(** [select_all t] returns all of the rows of the table [t]. 
    val select_all : unit -> t *)

(** [get_column t c] finds a column [col] in a table [t] and 
    returns the contents of that column. *)
val get_column : t -> string -> string list

(** [add_column t c] adds a column [col] to the end of a table [t] and returns
    a table [t'] with the added column. *)
val add_column : t -> string -> t

(** [remove_column t c] removes a column [col] from a table [t] and returns 
    a table without the specified column. 
    Raises: some error. *)
val remove_column : t -> string -> t

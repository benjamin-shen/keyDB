(** Represents a table. *)
(** A [Table] containing rows and columns. *)
type t

(** [empty] is the empty table. *)
val empty : t

(** [set_header t c] sets table [t]'s columns to [c]. *)
val set_columns : t -> string list -> t

(** [get_columns t] returns table [t]'s columns. *)
val get_columns : t -> string list

(** [read_insert_row t k r] adds a row [r] with key [k] to the end of a table [t]
    and returns the table with the added row. *)
val read_insert_row : t -> int -> Row.t -> t

(** [insert_row t r] adds a row [r] to the end of a table [t] and returns a
    table with the added row. *)
val insert_row : t -> Row.t -> t

(** [remove_row t k] removes a row with key [k] from a table [t] and returns a
    table without the specified row. *)
val remove_row : t -> int -> t

(** [insert_rows t k] will remove rows in [t] associated with the keys [k]. *)
val remove_rows : t -> int list -> t

(** [get_column t c] finds a column [c] in a table [t] and 
    returns the contents of that column. 
    Raises: Invalid_Column if [c] is not a column in table [t]. *)
val get_column : t -> string -> (int * string) list

(** [update_cell t k c v] will update the cell in [t] at key [k] and column [c]
    to value [v]. 
    Raises: Invalid_Key if [k] is not a valid key in table [t]. *)
val update_cell : t -> int -> string -> string -> t

(** [select c cd t] finds the rows in [t] for columns [c] that satisfy 
    the conditions [cd] and returns a table containing these columns and rows. 
    Raises: some error. *)
val select : string list -> Command.conditions -> t -> t

(** [add_columns t c] adds columns [cols] to the end of a table [t] and returns
    a table with the specified columns. *)
val add_columns : t -> string list -> t

(** [delete_columns t c] removes columns [cols] from a table [t] and returns 
    a table without the specified columns. 
    Raises: some error. *)
val delete_columns : t -> string list -> t

(** [to_csv t] converts table [t] into a csv string *)
val to_csv : t -> string
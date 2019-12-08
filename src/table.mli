(* Represents a table. *)

(** A [Table] containing rows and columns. *)
type t

(* TODO exception specs *)
exception ColumnExists of string
exception InvalidColumn of string
exception InvalidKey of string
exception TypeError

(** [empty] is the empty table. *)
val empty : t

(** [set_header t c] sets table [t]'s columns to [c]. *)
val set_columns : t -> string list -> t

(** [get_column_names t] returns table [t]'s columns. *)
val get_column_names : t -> string list

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
    Raises: InvalidColumn if [c] is not a column in table [t]. *)
val get_column : t -> string -> (int * string) list

(** [update_cell t k c v] will update the cell in [t] at key [k] and column [c]
    to value [v]. 
    Raises: InvalidKey if [k] is not a valid key in table [t]. *)
val update_cell : t -> int -> string -> string -> t

(** [select_all t] is the entire table [t]. *)
val select_all : t -> t

(** [select t c f] finds the rows in [t] that satisfy the conditions [c] and
    returns a table containing these rows.\
    Raises: some error. *)
val select : string list -> Command.conditions -> t -> t

(** [add_columns t c] adds columns [cols] to the end of a table [t] and returns
    a table with the specified columns. *)
val add_columns : t -> string list -> t

(** [delete_columns t c] removes columns [cols] from a table [t] and returns 
    a table without the specified columns. 
    Raises: some error. *)
val delete_columns : t -> string list -> t

(** [sum_column t c] sums column [col] in table [t] if each value are of
    type int or float. *)
val sum_column : t -> string -> string

(** [count t c] counts the non-null values of column [col] in table [t]. *)
val count : t -> string -> string

(** [count_null t c] counts the null values of column [col] in table [t]. *)
val count_null : t -> string -> string

(** [to_csv t] converts table [t] into a csv string *)
val to_csv : t -> string
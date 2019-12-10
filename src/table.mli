(**
   Table functions for the DBMS.
*)

(** Type [t] represents a table. *)
type t

(** Raised when column already exists. *)
exception ColumnExists of Command.column

(** Raised when column doesn't exist. *)
exception InvalidColumn of Command.column

(** Raised when key doesn't exist. *)
exception InvalidKey of Command.key

(** Raised when column values aren't all ints or all floats. *)
exception TypeError

(** [empty] is the empty table. *)
val empty : t

(** [set_columns t c] sets table [t]'s columns to [c]. *)
val set_columns : t -> Command.column list -> t

(** [get_column_names t] returns table [t]'s columns. *)
val get_column_names : t -> Command.column list

(** [read_insert_row t k r] adds a row [r] with key [k] to the end of table [t]
    and returns the table with the added row. *)
val read_insert_row : t -> Command.key -> Row.t -> t

(** [insert_row t r] adds a row [r] to the end of a table [t] and returns a
    table with the added row. *)
val insert_row : t -> Row.t -> t

(** [remove_rows t k] will remove rows in [t] associated with the keys [k]. *)
val remove_rows : t -> Command.key list -> t

(** [get_column t c] finds a column [c] in a table [t] and 
    returns the contents of that column. 

    Raises: [InvalidColumn c] if [c] is not a column in table [t]. *)
val get_column : t -> Command.column -> (Command.key * Command.value) list

(** [update_cell t k c v] will update the cell in [t] at key [k] and column [c]
    to value [v].

    Raises: [InvalidKey k] if [k] is not a valid key in table [t]. *)
val update_cell : t -> Command.key -> Command.column -> Command.value -> t

(** [add_columns t c] adds columns [cols] to the end of a table [t] and returns
    a table with the specified columns. *)
val add_columns : t -> Command.column list -> t

(** [delete_columns t c] removes columns [cols] from a table [t] and returns 
    a table without the specified columns. *)
val delete_columns : t -> Command.column list -> t

(** [select c cd t] finds the rows in [t] that satisfy the conditions [cd] and
    returns a table containing these rows with columns [c].

    Raises: [InvalidColumn col] if [c] or a column referred in [cd] 
    doesn't exist. *)
val select : Command.column list -> Command.condition list -> t -> t

(** [select_all cd t] finds the rows in [t] that satisfy the conditions [cd]. *)
val select_all : Command.condition list -> t -> t

(** [sum_column t c] sums column [col] in table [t] and returns it as a string.

    Raises: [TypeError] if all the values in [col] are not type int or float. *)
val sum_column : t -> Command.column -> string

(** [count t c] counts the non-null values of column [col] in table [t]. *)
val count : t -> Command.column -> string

(** [count_null t c] counts the null values of column [col] in table [t]. *)
val count_null : t -> Command.column -> string

(** [to_csv t] converts table [t] into a csv string. *)
val to_csv : t -> string
(** High level management for the DBMS *)

(** the type of database *)
type t

(** Raised when a table is not found. *)
exception TableNotFound of string

(** Raised when attempting to create a table that already exists. *)
exception TableExists of string

(** Raised when the csv is corrupted and can't be read as a table. *)
exception CorruptFile

(** [dir] is the directory of the database. *)
val dir : string

(** [create_table name cols] will build a new table file [name], with columns
    [cols]. 
    Raises: Table_Exists if [name] already exists. *)
val create_table : string -> string list -> string

(** [drop_table name] will remove the table associated with [name] from
    the database.
    Raises: TableNotFound if [name] does not exist. *)
val drop_table : string -> string

(** [read filename] will read the csv file with name [filename] and convert
    it into a Table structure. *)
val read : string -> Table.t

(** [write filename table] will look at a [table], convert it to a csv file, and 
    store it in the database directory with name [filename]. *)
val write : string -> Table.t -> string
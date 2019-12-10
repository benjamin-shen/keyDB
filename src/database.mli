(** 
   High level management for the DBMS. 
*)

(** Type [t] represents a database. *)
type t

(** Raised when a table is not found. *)
exception TableNotFound of string

(** Raised when attempting to create a table that already exists. *)
exception TableExists of string

(** Raised when attempting to create a table with duplicate columns. *)
exception DuplicateColumn of string

(** Raised when the csv can't be read as a table. *)
exception CorruptFile

(** [create_table name cols] will build a new table file [name], with columns
    [cols]. 

    Raises: [TableExists] if [name] already exists. 

    Raises: [DuplicateColumn] if [cols] has duplicates. *)
val create_table : string -> string list -> string

(** [drop_table name] will remove the table associated with [name] from
    the database.

    Raises: [TableNotFound] if [name] does not exist. *)
val drop_table : string -> string

(** [read filename] will read the csv file with name [filename] and convert
    it into a table.

    Raises: [CorruptFile] if [filename] can't be read. *)
val read : string -> Table.t

(** [write filename table] will convert a table [table] to a csv fileand 
    store it in the database directory with name [filename]. *)
val write : string -> Table.t -> string
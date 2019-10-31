(** High level management for the DBMS *)

(** the type of database *)
type t

(** Raised when a table is not found. *)
exception Table_Not_Found

(** Raised when attempting to create a table that already exists. *)
exception Table_Exists

(** [dir] is the directory of the database, likely databases. *)
val dir : string

(** [create_table name cols] will build a new table in a csv file, [name].csv 
 * the columns for this table will be the various strings within [cols]. *)
val create_table : string -> string list -> unit

(** [drop_table name] will remove the table associated with [name] from
 * the database. *)
val drop_table : string -> unit

(** [read filename] will read the csv file with name [filename] and convert
 * it into a Table structure. *)
val read : string -> Table.t 

(** [write table] will look at a [table], convert it to a csv file, and 
 * store it in the database directory. *)
val write : Table.t -> t
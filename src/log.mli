(** 
   Log user inputs to the DBMS.
*)

(** [clear] deletes the log. *)
val clear : unit -> unit

(** [write_log line] writes a command to the log. *)
val write_log : string -> unit

(** [get_log] returns a string of the log. *)
val get_log : unit -> string

(** [undo] undos the most recent logged command. *)
val undo: unit -> string
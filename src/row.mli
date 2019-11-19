(** Represents a row within a table. *)

(** The type of a row. *)
type t

(** [empty] is the empty row. *)
val empty : t 

(** [value r c] is the value in [r] at column [c]. *)
val value : t  -> string -> string

(** [add_column r c v] will add a column named [c] to row [r] with value [v]. *)
val add_column : t -> string -> string -> t

(** [update r c v] will update the value in row [r] at column [c] to [v]. *)
val update : t -> string -> string -> t

(** [to_hasht r] converts row [r] to a hashtable. *)
val to_hasht : t -> (string, string) Hashtbl.t 
(** Rows in a table implemented as an association list.*)


(** A [Row] contains keys and their associated values. *)
module type Row = sig

  (** [t] is the type of rows. *)
  type t = 'a * 'b list
  val update_cell : t -> c -> c



end
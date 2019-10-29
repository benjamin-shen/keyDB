(** Represents a table. *)

module type Table = sig

  (** A [Table] containing rows and columns. *)
  type t

  (** [row] is the type of the rows in [t]. *)
  type row

  (** [col] is the type of the columns in [t]. *)
  type col

  (** [insert_row t r] adds a row [row] to the end of a table [t] and returns a
      table [t'] with the added row. *)
  val insert_row : t -> row -> t

  (** [remove_row t r] removes a row [row] from a table [t] and returns a
      table [t'] without the specified row. *)
  val remove_row : t -> row -> t

  (** [select t r] finds and returns a row [row] in a table [t]. 
      Raises: some error. *)
  val select : t -> row -> row

  (** [select_all t] returns all of the rows of the table [t]. *)
  val select_all : t -> t

  (** [get_column t c] finds a column [col] in a table [t] and 
      returns the contents of that column. *)
  val get_column : t -> string -> col

  (** [add_column t c] adds a column [col] to the end of a table [t] and returns
      a table [t'] with the added column. *)
  val add_column : t -> col -> t

  (** [remove_column t c] removes a column [col] from a table [t] and returns 
      a table without the specified column. 
      Raises: some error. *)
  val remove_column : t -> col -> t

end
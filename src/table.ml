
type key = int
type column = string
type value = string

(** AF: An association list mapping keys to rows.
  * RI: All rows in t have the same length. *)
type t = (key * Row.t) list

let rep_ok t = 
  failwith "unimplemented"

let empty = 
  []

let insert_row t r =
  failwith "unimplemented"

let remove_row t r =
  failwith "unimplemented"

let select t c =
  failwith "unimplemented"

let get_column t c =
  failwith "unimplemented"

let add_column t c=
  failwith "unimplemented"

let remove_column t c=
  failwith "unimplemented"
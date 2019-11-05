
type key = int
type column = string
type value = string

(** AF: An association list mapping keys to rows.
  * RI: All rows in t have the same length. *)
type t = (key * Row.t) list

let rep_ok t = 
  t

let empty = 
  []

let insert_row t r =
  failwith "insert"

let remove_row t r =
  failwith "remove"

let select t c =
  failwith "select"

let get_column t c =
  failwith "get_column"

let add_column t c=
  failwith "add_column"

let remove_column t c=
  failwith "remove_column"
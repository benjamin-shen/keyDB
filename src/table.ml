
type key = int
type column = string
type value = string

(** AF: An association list mapping keys to rows, that 
    represents a database table.
  * RI: TODO *)
type t = (key * Row.t) list

let rep_ok t = 
  t

let empty = 
  []

let insert_row (t:t) k r : t =
  (k,r)::t

let remove_row t k =
  List.remove_assoc k t

let get_column t c =
  failwith "get_column"

let select t c =
  failwith "select"

let add_column t c=
  failwith "add_column"

let remove_column t c=
  failwith "remove_column"
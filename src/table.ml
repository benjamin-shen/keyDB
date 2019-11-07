type key = int
type column = string
type value = string
type condition = string

(** AF: An association list mapping keys to rows, that 
    represents a database table.
  * RI: TODO *)
type t = (key * Row.t) list

let rep_ok t = t

let empty = []

let header = ref []
let set_header columns = header := columns
let get_header = !header

let read_insert_row t k r =
  (k,r)::t

let key = ref 0
let insert_row t r =
  key := List.length t;
  (!key,r)::t

let remove_row t k =
  List.remove_assoc k t

let get_column t c =
  failwith "get_column"

let select t c =
  failwith "select"

let add_column t c =
  failwith "add_column"

let remove_column t c =
  failwith "remove_column"

let to_csv t =
  "to_csv not implemented"
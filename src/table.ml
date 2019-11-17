type key = int
type column = string
type value = string
type condition = string

(** AF: An association list mapping keys to rows, that 
    represents a database table.
  * RI: TODO *)
type t = {
  columns: column list;
  table: (key * Row.t) list
}

let rep_ok t = t

let empty = {
  columns = [];
  table = []
}

let get_columns t = t.columns

let read_insert_row t k r =
  {
    columns = t.columns;
    table = (k,r)::t.table
  }

let key = ref 0
let insert_row t r =
  key := List.length t.table;
  {
    columns = t.columns;
    table = (!key,r)::t.table
  }

let remove_row t k =
  { 
    columns = t.columns;
    table = List.remove_assoc k t.table
  }

let get_column t c =
  failwith "get_column"

let select t c =
  failwith "select"

let add_column t c =
  {
    columns = List.rev (c::(List.rev t.columns));
    table = t.table;
  }

let remove_column t c =
  failwith "remove_column"

(** [hasht_to_csv col hasht] converts a hashtable [hasht] to a csv string, 
    ordered by string list [col]. *)
let rec hasht_to_csv col hasht = 
  match col with
  | [] -> ""
  | h::t -> let tail = hasht_to_csv t hasht in 
    Hashtbl.find hasht h ^ (if tail="" then "" else ",") ^ tail

(** [string_row k r col] converts the row [r] to a string. *)
let string_row k r col =
  string_of_int k ^ "," ^ hasht_to_csv col (Row.to_hasht r)

(** [string_rows tab] converts the rows of a table to a string. *)
let rec string_rows tab = 
  let col = tab.columns in
  match tab.table with
  | [] -> ""
  | h::t -> let tail = string_rows {columns = col; 
                                    table = List.tl tab.table} in
    string_row (fst h) (snd h) col ^ (if tail="" then "" else "\n") ^ tail

(** [list_to_csv] converts a list to a csv string. *)
let rec list_to_csv = function
  | [] -> ""
  | h::t -> let tail = list_to_csv t in 
    h ^ (if tail="" then "" else ",")

let to_csv t =
  list_to_csv t.columns ^ "\n" ^ (string_rows t)
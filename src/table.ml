type key = int
type column = string
type value = string
type condition = string

exception Invalid_Column

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

let set_columns t c = {
  columns = c;
  table = t.table;
}

let get_columns t = t.columns

let read_insert_row t k r =
  {
    columns = t.columns;
    table = t.table @ [(k,r)]
  }

let key = ref 0
let insert_row t r =
  key := List.length t.table;
  {
    columns = t.columns;
    table = t.table @ [(!key,r)]
  }

let remove_row t k =
  { 
    columns = t.columns;
    table = List.remove_assoc k t.table
  }

let get_column t c =
  if not (List.mem c t.columns) then raise Invalid_Column else
    let rec table_builder acc = function
      | [] -> acc
      | (k, row)::t -> (k, Row.value row c)::acc
    in table_builder [] t.table

let select t c f =
  let col = get_column t c in
  try
    failwith ""
  with
    x -> failwith "exception occured"

let add_column t c =
  {
    columns = t.columns @ [c];
    table = t.table;
  }

let remove_column t c =
  failwith "remove_column"

(** [string_row k r col] converts the row [r] to a string. *)
let string_row k r col =
  string_of_int k ^ "," ^ Row.to_csv r

(** [string_rows tab] converts the rows of a table to a string. *)
let rec string_rows tab = 
  let col = tab.columns in
  match tab.table with
  | [] -> ""
  | h::t -> let tail = string_rows {columns = col;
                                    table = t} in
    string_row (fst h) (snd h) col ^ (if tail="" then "" else "\n") ^ tail

(** [list_to_csv] converts a list to a csv string. *)
let rec list_to_csv = function
  | [] -> ""
  | h::t -> let tail = list_to_csv t in 
    h ^ (if tail="" then "" else ",") ^ tail

let to_csv t =
  let r = "key," ^ list_to_csv t.columns ^ "\n" ^ (string_rows t) in
  (* print_endline r;  *)
  r
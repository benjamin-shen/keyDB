type key = int
type column = string
type value = string
type condition = string

exception InvalidColumn of string
exception InvalidKey of string
exception TypeError

(** AF: An association list mapping keys to rows, that 
    represents a database table.
  * RI: TODO *)
type t = {
  key: int;
  columns: column list;
  table: (key * Row.t) list
}

let rep_ok t = t

let empty = {
  key = -1;
  columns = [];
  table = []
}

let set_columns t c = {
  key = t.key;
  columns = c;
  table = t.table;
}

let get_column_names t = t.columns

let read_insert_row t k r =
  {
    key = k;
    columns = t.columns;
    table = t.table @ [(k,r)]
  }

let insert_row t r =
  {
    key = t.key + 1;
    columns = t.columns;
    table = t.table @ [(t.key + 1,r)]
  }

let remove_row t k =
  { 
    key = t.key;
    columns = t.columns;
    table = List.remove_assoc k t.table
  }

let rec remove_rows t = function
  | [] -> t
  | hd::tl -> remove_rows (remove_row t hd) tl

let get_column t c =
  if List.mem c t.columns then
    let rec table_builder table =
      match table with
      | [] -> []
      | (k, row)::t -> (k, Row.value row c)::table_builder t
    in table_builder t.table
  else raise (InvalidColumn c)

let update_cell t k c v = 
  if not (List.mem_assoc k t.table) 
  then raise (InvalidKey (string_of_int k)) else
    {
      key = t.key;
      columns = t.columns;
      table = t.table |> List.map 
                (fun x -> if not (fst x = k) then x else
                    ((fst x), Row.update (snd x) c v));
    }

let select_all t = t

let select t c f =
  let _ = get_column t c in
  try
    failwith ""
  with
    x -> failwith "exception occured"

let add_columns t c =
  {
    key = t.key;
    columns = t.columns @ c;
    table = t.table;
  }

let delete_columns t c =
  failwith "delete_columns"

(** [is_int i] returns whether string [s] can be converted to an int. *)
let is_int s =
  try ignore (int_of_string s); true
  with _ -> false

(** [is_int_col c] checks if association list [c] has string values that can 
    be converted to integers. *)
let rec is_int_col = function
  | [] -> true
  | (_,v)::t -> (is_int v) && (is_int_col t)

(** [sum_int i] sums an association list where each value is an integer. *)
let rec sum_int = function
  | [] -> 0
  | (_,v)::t -> (int_of_string v) + sum_int t

(** [is_float f] returns whether string [f] can be converted to a float. *)
let is_float s =
  try ignore (float_of_string s); true
  with _ -> false

(** [is_float_col c] checks if association list [c] has string values that can 
    be converted to floats. *)
let rec is_float_col = function
  | [] -> true
  | (_,v)::t -> (is_float v) && (is_float_col t)

(** [sum_float i] sums an association list where each value is an float. *)
let rec sum_float = function
  | [] -> 0.
  | (_,v)::t -> (float_of_string v) +. sum_float t

let sum_column t c =
  let col = get_column t c in
  if is_int_col col 
  then col
       |> sum_int 
       |> string_of_int 
  else if is_float_col col 
  then col
       |> sum_float 
       |> string_of_float
  else raise TypeError

(** [count_null c] counts the number of elements in list [c] with 
    an empty value. *)
let rec count_null = function
  | [] -> 0
  | (_,v)::t -> if v="" then 1 else 0 + count_null t

let count t c =
  let col = get_column t c in
  string_of_int (List.length col - count_null col)

let count_null t c =
  let col = get_column t c in
  string_of_int (count_null col)

(** [string_row k r col] converts the row [r] to a string. *)
let string_row k r col =
  string_of_int k ^ "," ^ Row.to_csv r

(** [string_rows tab] converts the rows of a table to a string. *)
let rec string_rows tab = 
  let col = tab.columns in
  match tab.table with
  | [] -> ""
  | h::t -> let tail = string_rows {key = tab.key; 
                                    columns = col;
                                    table = t} in
    string_row (fst h) (snd h) col ^ (if tail="" then "" else "\n") ^ tail

(** [list_to_csv] converts a list to a csv string. *)
let rec list_to_csv = function
  | [] -> ""
  | h::t -> let tail = list_to_csv t in 
    h ^ (if tail="" then "" else ",") ^ tail

let to_csv t =
  let rows = string_rows t in
  "key," ^ list_to_csv t.columns ^ if rows="" then rows else "\n" ^ rows
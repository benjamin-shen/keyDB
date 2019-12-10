exception ColumnExists of Command.column
exception InvalidColumn of Command.column
exception InvalidKey of Command.key
exception TypeError

(** A record holding the table's highest key, column names, and rows. *)
type t = {
  key: Command.key;
  columns: Command.column list;
  table: (Command.key * Row.t) list
}

(** [null] is the null value of a cell. *)
let null = "_"

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

let read_insert_row t k r = {
  key = k;
  columns = t.columns;
  table = t.table @ [(k,r)]
}

let insert_row t r = {
  key = t.key + 1;
  columns = t.columns;
  table = t.table @ [(t.key + 1,r)]
}

(** [remove_row t k] removes a row with key [k] from a table [t] and returns a
    table without the specified row. *)
let remove_row t k =  { 
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
  then raise (InvalidKey k) else
    {
      key = t.key;
      columns = t.columns;
      table = t.table |> List.map 
                (fun x -> if not (fst x = k) then x else
                    ((fst x), Row.update (snd x) c v));
    }

(** [add column t c] adds column [c] to table [t], filling the values 
    as null values. *)
let rec add_column tab col = 
  if List.mem col tab.columns then
    raise (ColumnExists col)
  else {
    key = tab.key;
    columns = tab.columns @ [col];
    table = List.map (fun (k, r) -> (k, Row.add_column r col null)) tab.table;
  }

let add_columns t c =
  let rec insert_columns table = function
    | [] -> table
    | h::t -> insert_columns (add_column table h) t
  in insert_columns t c

let delete_columns t c =
  let rec del_column table = function
    | [] -> table
    | h::t ->
      let rec column_from_rows col = function
        | [] -> []
        | (k,r)::tl -> 
          (k, Row.delete_column r col) :: column_from_rows col tl
      in 
      del_column (column_from_rows h table) t
  in 
  {
    key = t.key;
    columns = List.filter (fun x -> not (List.mem x c)) t.columns;
    table = del_column t.table c;
  }

let select c cd t = 
  try 
    (** [conditioned_table acc table] conditions table [table] to only contain
        rows satisfying the conditions. *)
    let rec conditioned_table (acc : (Command.key * Row.t) list) = function
      | [] -> acc
      | (k, r)::t -> 
        match Row.condition r c cd with
        | None -> conditioned_table acc t
        | Some row -> conditioned_table ((k, row)::acc) t
    in
    let table = conditioned_table (add_columns empty c).table t.table in 
    {
      key = if table = [] then 0 else table |> List.rev |> List.hd |> fst;
      columns = c;
      table = List.rev table;
    }
  with
  | Row.InvalidColumn col -> raise (InvalidColumn col)

let select_all cd t =
  select t.columns cd t

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

(** [lst_count_null c] counts the number of elements in list [c] with 
    an empty value. *)
let rec lst_count_null = function
  | [] -> 0
  | (_,v)::t -> (if v=null then 1 else 0) + lst_count_null t

let count t c =
  let col = get_column t c in
  string_of_int (List.length col - lst_count_null col)

let count_null t c =
  let col = get_column t c in
  string_of_int (lst_count_null col)

(** [string_row k r col] converts the row [r] to a string. *)
let string_row k r col =
  let row = Row.to_csv r in
  string_of_int k ^ (if row="" then "" else ",") ^ row

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
  let cols = list_to_csv t.columns in
  "key" ^ 
  (if cols="" then "" else ",") ^ cols ^ 
  if rows="" then "" else "\n" ^ rows
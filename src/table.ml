type key = int
type column = string
type value = string
type condition = string

exception Invalid_Column of string
exception Invalid_Key of string

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

let get_columns t = t.columns

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
  if not (List.mem c t.columns) then raise (Invalid_Column c) else
    let rec table_builder acc = function
      | [] -> acc
      | (k, row)::t -> (k, Row.value row c)::acc
    in table_builder [] t.table

let update_cell t k c v = 
  if not (List.mem_assoc k t.table) 
  then raise (Invalid_Key (string_of_int k)) else
    {
      key = t.key;
      columns = t.columns;
      table = t.table 
              |> List.map 
                (fun x -> if not (fst x = k) then x else
                    ((fst x), Row.update (snd x) c v));
    }

let rec add_column table col = 
  List.map (fun (k, r) -> (k, Row.add_column r col " ")) table

let add_columns t c =
  let rec insert_columns table = function
    | [] -> table
    | h::t ->
      insert_columns (add_column table h) t
  in
  {
    key = t.key;
    columns = t.columns @ c;
    table = (insert_columns t.table c);
  }

let delete_columns t c =
  let rec del_columns acc table = function
    | [] -> acc
    | h::t ->
      let rec column_from_rows acc col = function
        | [] -> acc
        | (k, r)::tl -> 
          column_from_rows ((k, Row.delete_column r col)::acc) col tl
      in 
      column_from_rows [] h table
  in 
  {
    key = t.key;
    columns = List.filter (fun x -> not (List.mem x c)) t.columns;
    table = del_columns [] t.table c;
  }

let select (c : column list) cd t = 
  try
    let rec conditioned_table (acc : (key * Row.t) list) = function
      | [] -> acc
      | (k, r)::t -> 
        match Row.condition r c cd with
        | None -> conditioned_table acc t
        | Some row -> conditioned_table ((k, row)::acc) t
    in
    let table = conditioned_table (add_columns empty c).table t.table in
    {
      key = table |> List.rev |> List.hd |> fst;
      columns = List.rev c;
      table = table |> List.rev;
    } 
  with
    x -> failwith "exception in select" 

(** [string_row k r col] converts the row [r] to a string. *)
let string_row k r col =
  string_of_int k ^ "," ^ Row.to_csv r

(** [string_rows tab] converts the rows of a table to a string. *)
let rec string_rows tab = 
  let col = tab.columns in
  match tab.table with
  | [] -> ""
  | h::t -> let tail = string_rows {key = tab.key; columns = col;
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
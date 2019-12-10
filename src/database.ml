type t = unit

exception TableNotFound of string
exception TableExists of string
exception DuplicateColumn of string
exception CorruptFile

(** [dir] is the directory where your tables are stored. *)
let dir = "databases"

(** [list_to_csv cols] takes a string list and returns
    its entries as a string to be input to a csv. The words will be
    in the same order as they were in the list. *)
let rec list_to_csv = function
  | [] -> ""
  | h::t -> let r = list_to_csv t in 
    if r = "" then h else h ^ "," ^ r

(** [dups_check checked lst] is the first duplicate found in [lst]. 
    If no duplicate is found, the empty string is returned. *)
let rec dups_check checked = function
  | [] -> ""
  | h::t -> if List.mem h checked then h else dups_check (h::checked) t

let create_table name cols = 
  let file = (dir ^ Filename.dir_sep ^ name) in
  if Sys.file_exists file then
    raise (TableExists name)
  else let s = (dups_check [] cols) in 
    if s = "" then begin
      ignore (Sys.command ("touch " ^ file));
      ignore (Sys.command 
                ({|echo "key,|} ^ list_to_csv cols ^ {|" > |} ^ file));
      "Created table: " ^ name end
    else 
      raise (DuplicateColumn s)

let drop_table name = 
  let table = (dir ^ Filename.dir_sep ^ name) in
  if Sys.file_exists table then begin
    ignore (Sys.command ("rm " ^ table));
    "Dropped table: " ^ name end
  else 
    raise (TableNotFound name)

(** [row_builder vals header acc] is the row with values [vals] associated 
    with columns in [header]. *)
let rec row_builder vals header acc = 
  if (List.length vals <= List.length header) then
    match vals with
    | [] -> acc
    | h::t -> 
      row_builder t (List.tl header) (Row.add_column acc (List.hd header) h)
  else 
    raise CorruptFile

(** [table_builder c header acc] creates a table by adding each line in [c] 
    to the table [acc]. *)
let rec table_builder c header acc = 
  try  
    let line = input_line c |> String.trim |> String.split_on_char ',' in 
    if line=[""] then 
      table_builder c header acc 
    else 
      table_builder c header 
        (Table.read_insert_row acc (int_of_string (List.hd line)) 
           (row_builder (List.tl line) header Row.empty))
  with
  | End_of_file -> Table.set_columns acc header

let read (filename : string) : Table.t =
  try
    let channel = open_in (dir ^ Filename.dir_sep ^ filename) in
    let header = List.tl (input_line channel |> String.split_on_char ',') in
    table_builder channel header Table.empty
  with
  | Sys_error _ -> raise (TableNotFound filename)
  | CorruptFile -> raise CorruptFile

let write filename table =
  let file = dir ^ Filename.dir_sep ^ filename in
  let csv = String.trim (Table.to_csv table) in
  ignore (Sys.command ("touch " ^ file));
  ignore (Sys.command ("echo " ^ "\"" ^ csv ^ "\"" ^ " > " ^ file));
  csv

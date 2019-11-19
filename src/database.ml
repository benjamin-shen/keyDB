open Hashtbl

type key = int
type column = string
type value = string

type t = unit

exception Table_Not_Found

exception Table_Exists

let dir = "databases"

(** [list_to_csv cols] takes a string list and returns
    its entries as a string to be input to a csv. The words will be
    in the same order as they were in the list. *)
let rec list_to_csv = function
  | [] -> ""
  | h::t -> let r = list_to_csv t in 
    if r = "" then h else h ^ "," ^ r

let create_table name cols = 
  let file = (dir ^ Filename.dir_sep ^ name) in
  if Sys.file_exists file then
    raise Table_Exists
  else
    ignore (Sys.command ("touch " ^ file));
  ignore (Sys.command ({|echo "key,|} ^ list_to_csv cols ^ {|" > |} ^ file));
  "Created table: " ^ name

let drop_table name = 
  let table = (dir ^ Filename.dir_sep ^ name) in
  if Sys.file_exists table then begin
    ignore (Sys.command ("rm " ^ table));
    "Dropped table: " ^ name end
  else 
    raise Table_Not_Found

(** [row_builder vals header acc] is the row with values [vals] associated 
    with columns in [header]. *)
let rec row_builder vals header acc = 
  if (List.length vals <= List.length header) then
    match vals with
    | [] -> acc
    | h::t -> 
      row_builder t (List.tl header) (Row.add_column acc (List.hd header) h)
  else failwith "Value list is too long."

(** [table_builder c header acc] will copy each line in [c] to the table [acc]. *)
let rec table_builder c header acc = 
  try  
    let line = input_line c |> String.split_on_char ',' in 
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
  | Sys_error _ -> raise Table_Not_Found
(*key,a,b,c
  0,0a,0b,0c
  1,1a,1b,1c
  2,2a,2b,2c*)

let write filename table =
  let file = dir ^ Filename.dir_sep ^ filename in
  let csv = Table.to_csv table in
  ignore (Sys.command ("touch " ^ file));
  ignore (Sys.command ("echo " ^ "\"" ^ csv ^ "\"" ^ " > " ^ file));
  csv

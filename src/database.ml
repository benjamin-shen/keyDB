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
  else begin
    ignore (Sys.command ("touch " ^ file));
    ignore (Sys.command ({|echo "key,|} ^ list_to_csv cols ^ {|" >> |} ^ file));
    "Created table: " ^ name end

let drop_table name = 
  let table = (dir ^ Filename.dir_sep ^ name) in
  if Sys.file_exists table then begin
    ignore (Sys.command ("rm " ^ table));
    "Dropped table: " ^ name end
  else 
    raise Table_Not_Found

let read (filename : string) : Table.t =

  (* [row_builder vals header acc] is the row with values [vals] associated 
   * with columns in [header]. *)
  let rec row_builder vals header acc = 
    assert (List.length vals <= List.length header); (* if else raise error *)
    match vals with
    | [] -> acc
    | h::t -> row_builder t (List.tl header) (Row.add_column acc hh hv)
    | _ -> failwith "vals and header lists not equal in row_builder. "
  in
  (* [table_builder c init] will copy each line in c to the table init. *)
  let rec table_builder c header init = 
    try  
      let line = input_line c |> String.split_on_char ',' in 
      table_builder c header 
        (Table.insert_row init (row_builder line header Row.empty))
    with
    | End_of_file -> init
  in

  try (* attempt to open file, call [table_builder] if present. *)
    let channel = open_in (dir ^ Filename.dir_sep ^ filename) in
    let header_row = input_line channel |> String.split_on_char ',' in
    table_builder channel Table.empty header_row
  with
  | Sys_error _ -> raise Table_Not_Found

let write name table =
  let file = (dir ^ Filename.dir_sep ^ name) in
  if Sys.file_exists file then
    raise Table_Exists
  else
    let cols = table |> Table.get_header |> String.concat ' ' in 
    create_table name cols

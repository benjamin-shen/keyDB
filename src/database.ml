open Hashtbl

type key = int
type column = string
type value = string

type t = unit

exception Table_Not_Found

exception Table_Exists

let dir = "databases"

(** [list_to_csv string list] takes a string list and returns
 * its entries as a string to be input to a csv. The words will be
 * in the same order as they were in the list. *)
let list_to_csv cols =   
  let rev = List.rev cols in 
  let rec tail_to_csv init = function
    | [] -> init
    | h::t -> h ^ "," ^ init
  in (tail_to_csv "" (List.tl rev)) ^ (List.hd rev)

let create_table name cols = 
  let file = (dir ^ Filename.dir_sep ^ name) in
  if Sys.file_exists file then
    raise Table_Exists
  else begin
    ignore (Sys.command ("touch " ^ file));
    ignore (Sys.command ({|echo "key,|} ^ list_to_csv cols ^ {|" >> |} ^ file));
    print_endline ("Created table: " ^ name) end

let drop_table name = 
  let table = (dir ^ Filename.dir_sep ^ name) in
  if Sys.file_exists table then begin
    ignore (Sys.command ("rm " ^ table));
    print_endline ("Dropped table: " ^ name) end
  else 
    raise Table_Not_Found

let read (filename : string) : Table.t =
  failwith ""

let write table =
  ()
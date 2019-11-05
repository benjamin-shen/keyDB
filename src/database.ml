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
  let handle = Unix.opendir dir 
  in
  (** [parse line] will parse a line into a string list. *)
  let parse line = 
    String.split_on_char ',' line 
  in
  (** [tbl_build handle] will add the header row and all the rows within
    * the file associacted with [handle] and add it to a table. *)
  let tbl_build handle =
    let channel = open_in 
let rec body init = 

in
in
(** [tbl_find] will find the file associated with [filename] in 
  * the parent function and call [tbl_build] to add all the contents to it. *)
let rec tbl_find = 
  let file = Unix.readdir handle in 
  if file = (filename ^ ".txt") then  
    (* if this is the file we are looking for *)
    tbl_build file Table.empty
  else
    tbl_find in
tbl_find

let write table =
  ()
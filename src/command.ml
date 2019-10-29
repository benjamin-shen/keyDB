type object_phrase = string list
type filename = string
type key = int
type column = string
type columns = string list
type values = string list
type conditions = string list

type command = 
  | Create of {file:filename;  (* create table cols*)
               cols:columns}
  | Drop   of filename         (* drop table *)
  | In     of filename*command (* in filename command *)
  | Where  of conditions       (* where condition(s) *)
  | Select of columns          (* select cols *)
  | Insert of values           (* insert vals *)
  | Remove of key              (* remove keyval  *)
  | Add    of column           (* add col *)
  | Delete of column           (* del col *)
  | Update of {key:key;        (* update keyval col newval *)
               col:column;
               value:string}
  | Sum    of column           (* sum col *)
  | Count  of column           (* count col *)
  | Count_Null of column       (* count_null col *)
  | Log
  | Undo
  | Quit

exception Empty

type err = string
exception Malformed of err

(** [get_command input] is the [input] without white space. *)
let rec get_command (input:string list) : string list =
  match input with
  | [] -> []
  | h::t -> if h="" then get_command t else h :: get_command t

(** [head input] is the first word of [input]. 
    Raises: [Empty] if [input] is empty. *)
let rec head (input:string list) : string =
  match input with
  | [] -> raise Empty
  | h::_ -> h

(** [tail input] is the tail of list [input]. *)
let rec tail (input:string list) : object_phrase =
  match input with
  | [] -> []
  | _::t -> t

let parse str =
  try 
    let cmd = get_command (String.split_on_char ' ' str) in
    let command_verb = head cmd in
    let object_phrase = tail cmd in
    let length = List.length object_phrase in
    if length=0 then
      match command_verb with
      | "quit" -> Quit
      | "log" -> Log
      | "undo" -> Undo
      | _ -> failwith "Empty object phrase."
    else
      match command_verb with
      | "create" -> 
        if length >= 2 
        then Create {
            file = head object_phrase;
            cols = tail object_phrase;
          } else failwith "`Create [filename] [column names]` failed."
      | "drop" -> 
        if length <> 1 
        then Drop (head object_phrase) else failwith "`Drop [filename]` failed."
      | _ -> failwith "Not a command."
  with 
  | Empty -> raise Empty
  | Failure err -> raise (Malformed err)
  | _ -> raise (Malformed "")
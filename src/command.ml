type object_phrase = string list

type command = 
  | In of object_phrase         (* command  keyword *)
  | Where of object_phrase      (* command  keyword *)
  | Select of object_phrase     (* display  data *)
  | Create of object_phrase     (* create table *)
  | Drop of object_phrase       (* drop table *)
  | Insert of object_phrase     (* insert row *)
  | Remove of object_phrase     (* remove row  *)
  | Add of object_phrase        (* add col *)
  | Delete of object_phrase     (* del col *)
  | Update of object_phrase     (* update cell *)
  | Sum of object_phrase        (* sum values *)
  | Count of object_phrase      (* count values *)
  | Count_Null of object_phrase (* count null values *)
  | Log    (* show log *)
  | Undo   (* reverse last command *)
  | Quit   (* quit the DBMS *)

exception Empty

exception Malformed

let list_to_command str_list = 
  match List.hd str_list with
  | "create" -> if List.length(List.tl str_list) = 0 
    then raise (Malformed) else Create (List.tl str_list)

  | "drop" -> if List.length(List.tl str_list) = 0 
    then raise (Malformed) else Drop (List.tl str_list)

  | _ -> raise (Malformed)

let parse str =
  if String.length str = 0 then raise (Empty) else
    str
    |> String.split_on_char ' '
    |> List.filter (fun x -> if String.length x = 0 then false else true)
    |> list_to_command
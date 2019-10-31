type object_phrase = string list
type filename = string
type key = int
type column = string
type values = string list
type conditions = string list

type table_command =
  | Select of column list*conditions
  | Insert of values
  | Remove of key list
  | Add    of column list
  | Delete of column list
  | Update of {key:key;
               col:column;
               value:string}
  | Sum    of column
  | Count  of column
  | Count_Null of column
type command = 
  | Log
  | Undo
  | Quit
  | Help
  | Create of {file:filename;
               cols:column list}
  | Drop   of filename
  | In     of filename*table_command

(* main commands *)
let log = "LOG"
let undo = "UNDO"
let quit = "QUIT"
(* database commands *)
let create = "CREATE [table] [cols]"
let drop = "DROP [table]"
(* table commands *)
let select = "IN [table] SELECT [cols] *? (WHERE [conditions])?"
let insert = "IN [table] INSERT [vals]"
let remove = "IN [table] REMOVE [keys]"
let add = "IN [table] ADD [cols]"
let delete = "IN [table] DELETE [cols]"
let update = "IN [table] UPDATE [key] [col] [val]"
let sum = "IN [table] SUM [col]"
let count = "IN [table] COUNT [col]"
let count_null = "IN [table] COUNT_NULL [col]"

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

let select_where (input:string list) =
  Select ([],[])

(** [table_command input] is the table command represented by [input]. *)
let table_command (input:string list) : table_command =
  let command_verb = head input in 
  let object_phrase = tail input in 
  let length = List.length object_phrase in 
  match command_verb with
  | "select" -> select_where object_phrase
  | "insert" -> Insert object_phrase
  | "remove" -> begin try 
        let keys = List.rev_map int_of_string object_phrase in Remove keys
      with _ -> failwith "Keys should be ints." end
  | "add" -> Add object_phrase
  | "delete" -> Delete object_phrase
  | "update" -> begin match object_phrase with
      | k::c::v::[] -> begin try
            let key = int_of_string k in Update {key;
                                                 col = c;
                                                 value = v}
          with _ -> failwith "Key is not an int." end
      | _ -> failwith "Incorrect update syntax." end
  | "sum" -> if length > 1 then failwith "sum"
    else Sum (head object_phrase)
  | "count" -> if length > 1 then failwith "count"
    else Count (head object_phrase)
  | "count_null" -> if length > 1 then failwith "count_null"
    else Count_Null (head object_phrase)
  | _ -> failwith "Not a table command."

let parse str =
  let str = Str.global_replace (Str.regexp "[^a-zA-Z0-9 .]+") "" str in
  let failure = {|"|} ^ str ^ {|"|} in
  try 
    let cmd = get_command 
        (String.split_on_char ' ' (String.lowercase_ascii str)) in
    let command_verb = head cmd in
    let object_phrase = tail cmd in
    let length = List.length object_phrase in
    if length=0 then
      match command_verb with
      | "quit" -> Quit
      | "help" -> Help
      | "log" -> Log
      | "undo" -> Undo
      | _ -> failwith "Empty object phrase."
    else
      match command_verb with
      | "create" -> if length < 2 then failwith "create"
        else Create {file = head object_phrase;
                     cols = tail object_phrase;}
      | "drop" -> if length = 1 then failwith "drop"
        else Drop (head object_phrase) 
      | "in" -> if length <= 2 then failwith "in"
        else In (head object_phrase, table_command (tail object_phrase))
      | _ -> failwith "Not a command."
  with 
  | Empty -> raise Empty
  | _ -> raise (Malformed failure)

let help () =
  "Database commands:\n" ^
  create ^ "\n  creates a new table [table] with column [cols].\n" ^
  drop ^ "\n  drops the table [table].\n" ^
  "\nTable commands:\n" ^
  select ^ "\n  prints the rows and columns [cols] in [table] 
  that satisfy [conditions]. * is equivalent to all columns.\n" ^
  insert ^ "\n  inserts a new row with [col] to [val] mappings.\n" ^
  remove ^ "\n  removes the row with key [key].\n" ^
  add ^ "\n  adds a new column [col].\n" ^
  delete ^ "\n  deletes the column [col].\n" ^
  update ^ "\n  updates the [col] mapping to [val] 
  in the row with key [key].\n" ^
  sum ^ "\n  sums [col], if possible.\n" ^
  count ^ "\n  counts the number of non-null cells in [col].\n" ^
  count_null ^ "\n  counts the number of null cells in [col].\n" ^
  "\nOther commands:\n" ^
  log ^ "\n  prints the log of the current session.\n" ^
  undo ^ "\n  undos the last command (there is no redo).\n" ^
  quit ^ "\n  quits the session."
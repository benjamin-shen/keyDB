type object_phrase = string list
type filename = string
type key = int
type column = string
type value = string
type operator = LT | LTE | EQ | NE | GT | GTE 
type condition = (column * operator * value)
type conditions = condition list

type table_command =
  | Select of column list*conditions
  | SelectStar of conditions
  | Insert of value list
  | Remove of key list
  | Add    of column list
  | Delete of column list
  | Update of {key:key;
               col:column;
               value:string}
  | Sum    of column
  | Count  of column
  | CountNull of column
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
let select = "IN [table] SELECT [cols] (WHERE [conditions])?"
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

let str_to_op = function 
    "<" -> LT | "<=" -> LTE | "=" | "==" -> EQ |
    ">" -> GT | ">=" -> GTE | "!=" | "!==" | "<>" -> NE |
    _ -> failwith "Invalid operator."

let rec list_to_conditions acc = function
  | [] -> acc
  | col::op::value::[] -> (col, str_to_op op, value)::acc
  | col::op::value::"&"::t -> 
    list_to_conditions ((col, str_to_op op, value)::acc) t
  | _ -> failwith "Invalid condition."

let select_where (input:string list) =
  match input with
  | [] -> failwith "select_where"
  | h::t -> 
    if h = "*" then
      match t with
      | [] | [_] -> SelectStar []
      | "where"::cond -> SelectStar (list_to_conditions [] cond)
      | _ -> failwith "No conditions were specified."
    else
      let rec select_builder acc boolcol = function
        | [] -> acc
        | h::t -> 
          if h = "where" then 
            select_builder acc false t
          else match acc with
            | Select (cols, conditions) -> 
              if boolcol then 
                select_builder (Select (cols@[h], conditions)) true t
              else if cols = [] then failwith "No columns were specified."
              else let conds = list_to_conditions [] (h::t) in
                (Select (cols, conds)) 
            | _ -> failwith "not select"
      in select_builder (Select ([],[])) true (h::t)

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
    else CountNull (head object_phrase)
  | _ -> failwith "Not a table command."

(** [has_dup lst] returns whether a list has duplicates. *)
let rec has_dup acc = function
  | [] | [_] -> false
  | h::t -> not (List.mem h acc) 
            && not (has_dup (h::acc) t)

let parse str =
  let clean = Str.global_replace (Str.regexp "[^a-zA-Z0-9* _.<>!=&]+") "" str in
  let str = Str.global_replace (Str.regexp "[ \n\r\x0c\t]+") " " clean in
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
      | "create" -> if length < 2 then failwith "A table need column names."
        else let cols = tail object_phrase in 
          if has_dup [] cols then failwith "A table needs unique column names."
          else Create {file = head object_phrase;
                       cols = tail object_phrase;}
      | "drop" -> if length <> 1 then failwith "drop"
        else Drop (head object_phrase) 
      | "in" -> if length <= 2 then failwith "in"
        else In (head object_phrase, table_command (tail object_phrase))
      | _ -> failwith "Not a command."
  with 
  | Empty -> raise Empty
  | _ -> raise (Malformed failure)

let help () =
  "Database commands:\n" ^
  create ^ "\n  creates a new table [table] with column [cols]. 
  The columns must be unique. \n" ^
  drop ^ "\n  drops the table [table].\n" ^
  "\nTable commands:\n" ^
  select ^ "\n  prints the columns [cols] of the rows in [table] 
  that satisfy [conditions]. * is equivalent to all columns.\n" ^
  insert ^ "\n  inserts a new row with values [vals].\n" ^
  remove ^ "\n  removes the rows with keys [keys].\n" ^
  add ^ "\n  adds new columns [cols].\n" ^
  delete ^ "\n  deletes columns [cols].\n" ^
  update ^ "\n  updates the [col] mapping to [val] 
  in the row with key [key].\n" ^
  sum ^ "\n  sums [col] if its values are of type int or float.\n" ^
  count ^ "\n  counts the number of non-null cells in [col].\n" ^
  count_null ^ "\n  counts the number of null cells in [col].
  There is an underscore between count and null.\n" ^
  "\nOther commands:\n" ^
  log ^ "\n  prints the log of the current session.\n" ^ (*
  undo ^ "\n  undos the last command (there is no redo).\n" ^ *)
  quit ^ "\n  quits the session."
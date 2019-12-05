(** [list_to_string lst] converts string list [lst] to a string with a space
    separating each element. *)
let rec list_to_string = function
  | [] -> ""
  | h::t -> h ^ " " ^ (list_to_string t)

(** [run_dbms] runs the DBMS. *)
let rec run_dbms () =
  print_newline ();
  print_string "> ";
  try
    let command = read_line () |> Command.parse in
    if command = Quit 
    then (print_newline (); 
          Log.clear (); print_string "Log cleared. ";
          print_endline {|Run "make clean" to delete all stored data.|}; exit 0) 
    else print_newline ();
    match command with
    | Quit -> failwith "should be handled"
    | Help -> print_endline (Command.help ()); run_dbms ()
    | Log -> let log = Log.get_log () in
      if log = "" then print_endline "Log is empty."
      else print_endline log; run_dbms ()
    | Undo -> print_endline (Log.undo ()); run_dbms () 
    | Create t -> begin 
        try print_endline (Database.create_table t.file t.cols);
          Log.write_log ("create " ^ t.file ^ " " ^ (list_to_string t.cols))
        with _ -> print_string ("Table " ^ t.file ^ " already exists. ");
          print_endline ({|To overwrite it, first do "drop |} ^ t.file ^ {|".|})
      end; run_dbms ()
    | Drop t -> begin
        try print_endline (Database.drop_table t);
          Log.write_log ("drop " ^ t)
        with _ -> 
          print_endline ("Table " ^ t ^ " does not exist.")
      end; run_dbms ()
    | In (file, Select (cols, conditions)) -> begin
        try 
          let old_table = Database.read file in 
          print_endline "Select with cond";
          old_table 
          |> Table.select cols conditions 
          |> Table.to_csv
          |> print_endline;
        with 
        | Database.TableNotFound -> 
          print_endline "Table not found"
      end; run_dbms ()
    | In (file, SelectStar) -> begin
        try 
          file 
          |> Database.read 
          |> Table.select_all
          |> Table.to_csv 
          |> print_endline;
        with 
        | Database.TableNotFound -> 
          print_endline "Table not found."
      end; run_dbms ()
    | In (file, Insert vals) -> begin 
        try
          let table = Database.read file in 
          vals 
          |> Row.build_row (Table.get_column_names table) 
          |> Table.insert_row table
          |> Database.write file 
          |> print_endline;
          Log.write_log ("in " ^ file ^ " insert " ^ (list_to_string vals))
        with 
        | Database.TableNotFound -> 
          print_endline "Table not found."
      end; run_dbms ()
    | In (file, Remove keys) -> begin
        try
          let table = Database.read file in
          Table.remove_rows table keys 
          |> Database.write file 
          |> print_endline;
          Log.write_log ("in " ^ file ^ " remove " ^ 
                         (keys |> List.map string_of_int |> list_to_string))
        with 
        | Database.TableNotFound -> 
          print_endline "Table not found."
      end; run_dbms ()
    | In (file, Update {key=k;
                        col=c;
                        value=v}) -> begin
        try 
          let table = Database.read file in
          Table.update_cell table k c v
          |> Database.write file
          |> print_endline;
          Log.write_log
            ("in " ^ file ^ " update " ^ (string_of_int k) ^ " " ^ c ^ " " ^ v)
        with 
        | Database.TableNotFound -> print_endline "Table not found."
      end; run_dbms ()
    | In (file, Add columns) -> begin
        (* BUG: does not add commas when writing to csv. *)
        print_endline "add cols";
        try 
          let table = Database.read file in 
          Table.add_columns table columns
          |> Database.write file
          |> print_endline
        with
        | Database.TableNotFound ->
          print_endline "Table not found"
      end; run_dbms () 
    | In (file, Delete columns) -> begin
        print_endline "del cols";
        try 
          let table = Database.read file in
          Table.delete_columns table columns
          |> Database.write file
          |> print_endline
        with
        | Database.TableNotFound ->
          print_endline "Table not found" 
      end; run_dbms ()
    | In (file, Sum col) -> begin
        try 
          let table = Database.read file in
          print_endline (Table.sum_column table col);
        with 
        | Database.TableNotFound -> 
          print_endline "Table not found."
        | Table.TypeError -> 
          print_endline "Column values must be ints/floats."
        | Table.InvalidColumn c -> 
          print_endline ("Invalid column \"" ^ c ^ "\".")
      end; run_dbms ()
    | In (file, Count col) -> begin
        try 
          let table = Database.read file in
          print_endline (Table.count table col);
        with 
        | Database.TableNotFound -> 
          print_endline "Table not found."
        | Table.InvalidColumn c -> 
          print_endline ("Invalid column \"" ^ c ^ "\".")
      end; run_dbms ()
    | In (file, CountNull col) -> begin
        try 
          let table = Database.read file in
          print_endline (Table.count_null table col);
        with 
        | Database.TableNotFound -> 
          print_endline "Table not found."
        | Table.InvalidColumn c -> 
          print_endline ("Invalid column \"" ^ c ^ "\".")
      end; run_dbms ()
  with 
  | Command.Empty ->
    print_newline ();
    print_string ({|Error: no command was recognized. |});
    print_endline {|Type "help" or "quit".|};
    run_dbms ()
  | Command.Malformed str ->
    print_newline ();
    print_string ("Error: " ^ str ^ " is not a valid command. ");
    print_endline {|Type "help" or "quit".|};
    run_dbms ()
  | _ ->
    print_newline ();
    print_newline ();
    print_endline {|Error. Type "help" or "quit".|};
    run_dbms ()

(** [main ()] prints initial instructions, then runs the DBMS. *)
let main () =
  ANSITerminal.(print_string [green]
                  "\nWelcome to keyDB, a DBMS that handles SQL queries.\n");
  print_endline {|Type "help" to see all the available commands.|};
  run_dbms ()

(* Execute the dbms engine. *)
let () = main ()

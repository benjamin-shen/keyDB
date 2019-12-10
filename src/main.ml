(**
   The main engine for keyDB, 
   a console-based Database Management System (DBMS) built in OCaml.
*)

(** [list_to_string lst] converts string list [lst] to a string with a space
    separating each element. *)
let rec list_to_string = function
  | [] -> ""
  | h::t -> h ^ " " ^ (list_to_string t)

(** [run_dbms] runs the DBMS in a REPL. *)
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
        print_endline (Database.create_table t.file t.cols);
        Log.write_log ("create " ^ t.file ^ " " ^ (list_to_string t.cols))
      end; run_dbms ()
    | Drop t -> begin
        print_endline (Database.drop_table t);
        Log.write_log ("drop " ^ t)
      end; run_dbms ()
    | In (file, Select (cols, conditions)) -> begin
        file
        |> Database.read 
        |> Table.select cols conditions 
        |> Table.to_csv
        |> print_endline
      end; run_dbms ()
    | In (file, SelectStar conditions) -> begin
        file 
        |> Database.read 
        |> Table.select_all conditions
        |> Table.to_csv 
        |> print_endline;
      end; run_dbms ()
    | In (file, Insert vals) -> begin 
        let table = Database.read file in 
        vals 
        |> Row.build_row (Table.get_column_names table) 
        |> Table.insert_row table
        |> Database.write file 
        |> print_endline;
        Log.write_log ("in " ^ file ^ " insert " ^ (list_to_string vals))
      end; run_dbms ()
    | In (file, Remove keys) -> begin
        let table = Database.read file in
        Table.remove_rows table keys 
        |> Database.write file 
        |> print_endline;
        Log.write_log ("in " ^ file ^ " remove " ^ 
                       (keys |> List.map string_of_int |> list_to_string))
      end; run_dbms ()
    | In (file, Update {key=k;
                        col=c;
                        value=v}) -> begin
        let table = Database.read file in
        Table.update_cell table k c v
        |> Database.write file
        |> print_endline;
        Log.write_log
          ("in " ^ file ^ " update " ^ (string_of_int k) ^ " " ^ c ^ " " ^ v)
      end; run_dbms ()
    | In (file, Add columns) -> begin
        let table = Database.read file in 
        Table.add_columns table columns
        |> Database.write file
        |> print_endline;
        Log.write_log
          ("in " ^ file ^ " add " ^ (list_to_string columns))
      end; run_dbms () 
    | In (file, Delete columns) -> begin
        let table = Database.read file in
        Table.delete_columns table columns
        |> Database.write file
        |> print_endline;
        Log.write_log
          ("in " ^ file ^ " delete " ^ (list_to_string columns))
      end; run_dbms ()
    | In (file, Sum col) -> begin
        let table = Database.read file in
        print_endline (Table.sum_column table col)
      end; run_dbms ()
    | In (file, Count col) -> begin
        let table = Database.read file in
        print_endline (Table.count table col);
      end; run_dbms ()
    | In (file, CountNull col) -> begin
        let table = Database.read file in
        print_endline (Table.count_null table col);
      end; run_dbms ()
  with 
  | Table.TypeError -> 
    print_endline "Column values must be non-null ints/floats.";
    run_dbms ()
  | Table.InvalidKey k -> 
    print_endline ("The key: " ^ k ^ " is invalid.");
    run_dbms ()
  | Table.InvalidColumn c -> 
    print_endline ("Invalid column " ^ c ^ ".");
    run_dbms ()
  | Table.ColumnExists c ->
    print_endline ("Column " ^ c ^ " already exists.");
    run_dbms ()
  | Row.ValueMismatch n ->
    if n = 0 then begin
      print_string "You cannot insert values in a table with no columns. ";
      print_endline "Add columns or drop this table."
    end
    else begin
      print_string "Incorrect number of values. ";
      if n = 1 then
        print_string "There is 1 column. "
      else
        print_string ("There are " ^ string_of_int n ^ " columns. ");
      print_endline "Use an underscore for null values.";
    end; run_dbms ()
  | Database.TableNotFound file -> 
    print_endline ("Table " ^ file ^ " not found.");
    run_dbms ()
  | Database.TableExists file -> 
    print_string ("Table " ^ file ^ " already exists. ");
    print_endline ({|To overwrite it, first do "drop |} ^ file ^ {|".|});
    run_dbms ()
  | Database.DuplicateColumn col -> 
    print_string ("Column " ^ col ^ " is repeated twice. ");
    print_endline 
      ({|Please attempt to create the table with a different column name.|});
    run_dbms ()
  | Database.CorruptFile ->
    print_endline "The file is corrupted and can't be read as a table.";
    run_dbms ()
  | Command.Empty ->
    print_newline ();
    print_string "Error: no command was recognized. ";
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

(** [main] prints initial instructions, then runs the DBMS. *)
let main () =
  ANSITerminal.(print_string [green]
                  "\nWelcome to keyDB, a DBMS that handles SQL queries.\n");
  print_endline {|Type "help" to see all the available commands.|};
  run_dbms ()

(* Execute the main engine. *)
let () = main ()

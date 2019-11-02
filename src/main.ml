(** [run_dbms] runs the DBMS. *)
let rec run_dbms () =
  print_newline ();
  print_string "> ";
  try
    let command = read_line () |> Command.parse in
    if command = Quit 
    then (print_newline (); print_string "Log cleared. ";
          print_endline {|Run "make clean" to delete all stored data.|}; exit 0) 
    else print_newline ();
    match command with
    | Help -> print_endline (Command.help ()); run_dbms ()
    | Log -> print_endline (Log.get_log ()); run_dbms ()
    | Undo -> print_endline "Undo not implemented."; run_dbms ()
    | Create t -> begin 
        try Database.create_table t.file t.cols
        with _ -> print_string ("Table " ^ t.file ^ " already exists. ");
          print_endline ({|To overwrite it, first do "drop |} ^ t.file ^ {|".|})
      end; run_dbms ()
    | Drop t -> begin
        try Database.drop_table t
        with _ -> print_endline ("Table " ^ t ^ " does not exist.")
      end; run_dbms ()
    | _ -> print_endline "Command not implemented."; run_dbms ()
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

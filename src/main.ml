(** [run_dbms] runs the DBMS. *)
let rec run_dbms () =
  print_newline ();
  try
    let command = read_line () |> Command.parse in
    match command with
    | Quit -> exit 0
    | _ -> run_dbms ()
  with _ ->
    run_dbms ()

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\nWelcome to keyDB, a DBMS that handles SQL queries.\n");
  print_endline {|Type "help" to see available commands.|};
  run_dbms ()

(* Execute the game engine. *)
let () = main ()

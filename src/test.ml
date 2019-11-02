open OUnit2
open Database

let database_tests = 
  [
    "create drop table test" >:: (fun _ -> 
        (try ignore (create_table "test" ["t";"e";"st"]); ()
         with _ -> failwith "create");
        (try ignore (drop_table "test"); ()
         with _ -> failwith "drop");
      );
    "fail create test" >:: (fun _ -> 
        (try ignore (create_table "test" ["t";"e";"st"]);
           ignore (create_table "test" ["t";"e";"s";"t"]);
           ignore (drop_table "test");
           failwith "create";
         with Table_Exists -> ();
           ignore (drop_table "test"));
      );
    "fail drop test" >:: (fun _ -> 
        (try ignore (drop_table "test"); (* Sys will print out error *)
           failwith "drop";
         with Table_Not_Found -> ());
      );
  ]

let table_tests = 
  [
    "name" >:: (fun _ -> ());
  ]

let row_tests = 
  [
    "name" >:: (fun _ -> ());
  ]

let log_tests = 
  [
    "name" >:: (fun _ -> ());
  ]

let command_tests = 
  [
    "name" >:: (fun _ -> ());
  ]

let suite =
  "test suite for keyDB"  >::: List.flatten [
    database_tests;
    table_tests;
    row_tests;
    log_tests;
    command_tests;
  ]

let _ = run_test_tt_main suite

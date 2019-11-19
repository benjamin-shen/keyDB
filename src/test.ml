open OUnit2
open Database
open Table

let database_tests = 
  [
    "newline" >:: (fun _ -> print_newline (););
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
    "read test" >:: (fun _ -> 
        (try ignore (create_table "test" ["t";"e";"st"]); ()
         with _ -> failwith "create");

        ignore (drop_table "test");
      );
  ]

let table_tests = 
  [
    "newline" >:: (fun _ -> print_newline (););
    "to_csv test" >:: (fun _ -> 
        let row_a = Row.add_column (Row.empty) "col" "a" in
        let row_b = Row.add_column (Row.empty) "col" "b" in
        let t = insert_row (add_column Table.empty "col") row_a in
        print_endline (to_csv t);
        assert_equal (to_csv t) "key,col\n0,a";
        print_endline (to_csv (insert_row (add_column t "col") row_b));
      );
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

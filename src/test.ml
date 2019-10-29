open OUnit2

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
    table_tests;
    row_tests;
    log_tests;
    command_tests;
  ]

let _ = run_test_tt_main suite

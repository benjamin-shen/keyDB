open OUnit2

let _tests = 
  [
    "name" >:: (fun _ -> ());
  ]

let suite =
  "test suite for keyDB"  >::: List.flatten [
    _tests;
  ]

let _ = run_test_tt_main suite

open OUnit2
open Database
open Table


let f3 f a b t = f t a b
let f2 f a t = f t a

let database_tests =  [
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
       with TableExists table -> ();
         ignore (drop_table "test"));
    );
  "fail drop test" >:: (fun _ -> 
      (try ignore (drop_table "test"); (* Sys will print out error *)
         failwith "drop";
       with TableNotFound table -> ());
    );
  "read test" >:: (fun _ -> 
      (try ignore (create_table "test" ["t";"e";"st"]); ()
       with _ -> failwith "create");

      ignore (drop_table "test");
    );
]

let row_empty =          Row.empty
let row_a =              Row.empty |> f3 (Row.add_column) "a" "a" 
let row_a_update_b =     Row.empty |> f3 (Row.add_column) "a" "b" 
let row_ab =             Row.empty 
                         |> f3 (Row.add_column) "a" "a"
                         |> f3 (Row.add_column) "b" "b"
let cond_row_a =         Row.condition row_a ["a"]
let cond_row_ab_a =      Row.condition row_ab ["a"]
let cond_row_ab_ab =     Row.condition row_ab ["a";"b"]
let cond_row_ab_b =      Row.condition row_ab ["b"]
let conds_a_lt_b_t =     [("a", Command.LT, "b")]
let conds_a_lt_b_f =     [("a", Command.LT, "0")]
let conds_a_lte_b_t =    [("a", Command.LTE, "b")]
let conds_a_lte_b_t_eq = [("a", Command.LTE, "a")]
let conds_a_lte_b_f =    [("a", Command.LTE,"0")]
let conds_a_eq_a_t =     [("a", Command.EQ, "a")]
let conds_a_eq_b_f =     [("a", Command.EQ, "b")]
let conds_a_ne_b_t =     [("a", Command.NE, "b")]
let conds_a_ne_a_f =     [("a", Command.NE, "a")]
let conds_a_gt_b_f =     [("a", Command.GT, "b")]
let conds_a_gt_b_t =     [("a", Command.GT, "0")]
let conds_a_gte_b_f =    [("a", Command.GTE, "b")]
let conds_a_gte_b_t_eq = [("a", Command.GTE, "a")]
let conds_a_gte_b_t =    [("a", Command.GTE,"0")]
let conds_mult_t_t =     [("a", Command.LT, "b"); ("a", Command.EQ, "a")]
let conds_mult_t_f =     [("a", Command.LT, "b"); ("a", Command.EQ, "b")]
let conds_mult_f_f =     [("a", Command.LT, "0"); ("a", Command.EQ, "b")]
let row_tests = [
  "value: col a in row_a is a" >:: (fun _ ->
      assert_equal (Row.value row_a "a") "a";);
  "value: col a in row_ab is a" >:: (fun _ ->
      assert_equal (Row.value row_ab "b") "b");
  "adding col: adding column b in row_a is row_ab" >:: (fun _ ->
      assert_equal (Row.add_column row_a "b" "b") row_ab;);
  "deleting col: column b in row_ab is row_a" >:: (fun _ ->
      assert_equal (Row.delete_column row_ab "b") row_a;);
  "deleting col: column a in row_a is row_empty" >:: (fun _ ->
      assert_equal (Row.delete_column row_a "a") row_empty;);
  "updating val: column a in row_a is b" >:: (fun _ ->
      assert_equal (Row.update row_a "a" "b") row_a_update_b;);
  "building row: columns a b with values a b is row_ab" >:: (fun _ ->
      assert_equal (Row.build_row ["a";"b"] ["a";"b"]) row_ab;);
  "to_csv row one val: to_csv of row_a" >:: (fun _ ->
      assert_equal (Row.to_csv row_a) "a";);
  "to_csv row mult vals: to_csv of row_ab" >:: (fun _ ->
      assert_equal (Row.to_csv row_ab) "a,b";);
  "to_csv row no vals: to_csv of row_a" >:: (fun _ ->
      assert_equal (Row.to_csv row_empty) "";);
  (* Condition Testing *)
  "condition: LT, true, one cond; row_a, a < b" >:: (fun _ ->
      assert_equal (cond_row_a conds_a_lt_b_t) (Some row_a););
  "condition: LT, false, one cond; row_a, a < 0" >:: (fun _ ->
      assert_equal (cond_row_a conds_a_lt_b_f) None;);
  "condition: LTE, true, one cond; row_a, a <= b (less than)" >:: (fun _ ->
      assert_equal (cond_row_a conds_a_lte_b_t) (Some row_a););
  "condition: LTE, true, one cond; row_a, a <= a (equal)" >:: (fun _ ->
      assert_equal (cond_row_a conds_a_lte_b_t_eq) (Some row_a););
  "condition: LTE, false, one cond; row_a, a <= 0" >:: (fun _ ->
      assert_equal (cond_row_a conds_a_lte_b_f) None;);
  "condition: EQ, true, one cond; row_a, a = a" >:: (fun _ ->
      assert_equal (cond_row_a conds_a_eq_a_t) (Some row_a););
  "condition: EQ, false, one cond; row_a, a = b" >:: (fun _ ->
      assert_equal (cond_row_a conds_a_eq_b_f) None;);
  "condition: NE, true, one cond; row_a, a <> b" >:: (fun _ ->
      assert_equal (cond_row_a conds_a_ne_b_t) (Some row_a););
  "condition: NE, false, one cond; row_a, a <> a" >:: (fun _ ->
      assert_equal (cond_row_a conds_a_ne_a_f) None;);
  "condition: GT, true, one cond; row_a, a > 0" >:: (fun _ ->
      assert_equal (cond_row_a conds_a_gt_b_t) (Some row_a););
  "condition: GT, false, one cond; row_a, a > b" >:: (fun _ ->
      assert_equal (cond_row_a conds_a_gt_b_f) None;);
  "condition: GTE, true, one cond; row_a, a >= 0 (greater than)" >:: (fun _ ->
      assert_equal (cond_row_a conds_a_gte_b_t) (Some row_a););
  "condition: GTE, true, one cond; row_a, a >= a (equal)" >:: (fun _ ->
      assert_equal (cond_row_a conds_a_gte_b_t_eq) (Some row_a););
  "condition: GTE, false, one cond; row_a, a >= b" >:: (fun _ ->
      assert_equal (cond_row_a conds_a_gte_b_f) None;);
  "conditions: LT - true, EQ - true, two conds; a < b, a = a" >:: (fun _ ->
      assert_equal (cond_row_a conds_mult_t_t) (Some row_a););
  "conditions: LT - true, EQ - false, two conds; a < b, a = b" >:: (fun _ ->
      assert_equal (cond_row_a conds_mult_t_f) None;);
  "conditions: LT - false, EQ - false, two conds; a < 0, a = b" >:: (fun _ ->
      assert_equal (cond_row_a conds_mult_f_f) None;);
  "condition: No conditions, select a from a." >:: (fun _ ->
      assert_equal (cond_row_a []) (Some row_a););
  "condition: No conditions, select a from ab." >:: (fun _ ->
      assert_equal (cond_row_ab_a []) (Some row_a););

  (* INCOMPLETE TESTING AREA *)
  "condition: select one col, condition another" >:: (fun _ ->
      assert_equal (cond_row_a conds_mult_f_f) None;); 


  "condition: conditioning invalid column in condition." >:: (fun _ ->
      let inval_call = fun () -> cond_row_a [("c", Command.EQ,"c")] in
      assert_raises (Row.InvalidColumn "c") inval_call;);
  "condition: Invalid column in column selection, w/ conditions" >:: (fun _ ->
      let inval_call = fun () -> (Row.condition row_a ["d"]) conds_a_eq_a_t in
      assert_raises (Row.InvalidColumn "d") inval_call;);
  "condition: Invalid column in column selection, no conditions" >:: (fun _ ->
      let inval_call = fun () -> (Row.condition row_a ["d"]) [] in
      assert_raises (Row.InvalidColumn "d") inval_call;);
]

let table_empty = Table.empty
let table_ab_1 = Table.empty |> f2 insert_row row_ab
let row0 = Row.empty 
           |> f3 (Row.add_column) "a" "0a"
           |> f3 (Row.add_column) "b" "0b"
           |> f3 (Row.add_column) "c" "0c"
let row1 = Row.empty 
           |> f3 (Row.add_column) "a" "1a"
           |> f3 (Row.add_column) "b" "1b"
           |> f3 (Row.add_column) "c" "1c"
let row2 = Row.empty 
           |> f3 (Row.add_column) "a" "2a"
           |> f3 (Row.add_column) "b" "2b"
           |> f3 (Row.add_column) "c" "2c"
let table_abc_3 = Table.empty |> f2 add_columns ["a";"b";"c"]
                  |> f2 insert_row row0 
                  |> f2 insert_row row1 
                  |> f2 insert_row row2
let table_tests = [
  "set columns: set empty columns to a, b, c" >:: (fun _ -> 
      assert_equal ["a";"b";"c"] (Table.set_columns table_empty ["a";"b";"c"] 
                                  |> Table.get_column_names););
  "to_csv test" >:: (fun _ -> 
      let row_a = Row.add_column (Row.empty) "col" "a" in
      let row_b = Row.add_column (Row.empty) "col" "b" in
      let t = insert_row (Table.add_columns Table.empty ["col"]) row_a in
      assert_equal (to_csv t) "key,col\n0,a";
      assert_equal (to_csv (insert_row t row_b)) "key,col\n0,a\n1,b";
    );
]

let log_tests = [
  "name" >:: (fun _ -> ());
]

open Command
let command_tests = [
  (* General Commands *)
  {|Parse "log"|} >:: (fun _  -> assert_equal (parse "log")  Log);
  {|Parse "undo"|} >:: (fun _ -> assert_equal (parse "undo") Undo);
  {|Parse "quit"|} >:: (fun _ -> assert_equal (parse "quit") Quit);
  {|Parse "help"|} >:: (fun _ -> assert_equal (parse "help") Help);
  {|Parse "create abc a b c"|} >:: (fun _ -> 
      assert_equal (parse "create abc a b c") (Create {file="abc"; 
                                                       cols=["a";"b";"c"]}));
  {|Parse "create"|} >:: (fun _ -> 
      let c = fun () -> parse "create" in 
      assert_raises (Malformed {|"create"|}) c);
  {|Parse "create abc"|} >:: (fun _ -> 
      let c = fun () -> parse "create abc" in
      assert_raises (Malformed {|"create abc"|}) c);
  {|Parse "drop abc"|} >:: (fun _ -> 
      assert_equal (parse "drop abc") (Drop "abc"));
  {|Parse "drop"|} >:: (fun _ -> 
      let c = fun () -> parse "drop" in
      assert_raises (Malformed {|"drop"|}) c);

  (* Table Commands *)
  {|Parse "in abc select a where a > 0"|} >:: (fun _ -> 
      assert_equal (parse "in abc select a where a > 0") 
        (In ("abc", Select (["a"],[("a",GT,"0")]))));
  {|Parse "in abc select a where"|} >:: (fun _ -> 
      assert_equal (parse "in abc select a where") 
        (In ("abc", Select (["a"],[]))));
  {|Parse "in abc select a b where a > 0"|} >:: (fun _ -> 
      assert_equal (parse "in abc select a b where a > 0") 
        (In ("abc", Select (["a";"b"],[("a",GT,"0")]))));
  {|Parse "in abc select a where a > 0 & a = 1"|} >:: (fun _ -> 
      assert_equal (parse "in abc select a where a > 0 & a = 1")
        (In ("abc", Select (["a"],[("a",EQ,"1");("a",GT,"0")]))) 
    );
  {|Parse "in abc select a"|} >:: (fun _ -> 
      assert_equal (parse "in abc select a") 
        (In ("abc", Select (["a"],[]))));
  {|Parse "in abc select a b"|} >:: (fun _ -> 
      assert_equal (parse "in abc select a b") 
        (In ("abc", Select (["a";"b"],[]))));

  {|Parse "in abc select where a > 0 & a = 1"|} >:: (fun _ -> 
      let c = fun () -> parse "in abc select where a > 0 & a = 1" in
      assert_raises (Malformed {|"in abc select where a > 0 & a = 1"|}) c);
  {|Parse "in abc select a where a > 0 & a ="|} >:: (fun _ -> 
      let c = fun () -> parse "in abc select a where a > 0 & a =" in
      assert_raises (Malformed {|"in abc select a where a > 0 & a ="|}) c);
  {|Parse "in abc select"|} >:: (fun _ -> 
      let c = fun () -> parse "in abc select" in
      assert_raises (Malformed {|"in abc select"|}) c);

  {|Parse "in abc select *"|} >:: (fun _ -> 
      assert_equal (parse "in abc select *") 
        (In ("abc", SelectStar [])));
  {|Parse "in abc select * where a > 0"|} >:: (fun _ -> 
      assert_equal (parse "in abc select * where a > 0") 
        (In ("abc", SelectStar [("a",GT,"0")])));
  {|Parse "in abc select * where"|} >:: (fun _ -> 
      assert_equal (parse "in abc select * where") 
        (In ("abc", SelectStar [])));

  {|Parse "in abc insert a b c"|} >:: (fun _ -> 
      assert_equal (parse "in abc insert a b c") 
        (In ("abc", Insert ["a";"b";"c"])));
  {|Parse "in abc insert"|} >:: (fun _ -> 
      let c = fun () -> parse "in abc select" in
      assert_raises (Malformed {|"in abc select"|}) c); 
  {|Parse "in abc remove 0"|} >:: (fun _ -> 
      assert_equal (parse "in abc remove 0") (In ("abc", Remove [0])));
  {|Parse "in abc remove 0 1"|} >:: (fun _ -> 
      assert_equal (parse "in abc remove 0 1") (In ("abc", Remove [1;0])));
  {|Parse "in abc remove"|} >:: (fun _ -> 
      let c = fun () -> parse "in abc remove" in
      assert_raises (Malformed {|"in abc remove"|}) c);
  {|Parse "in abc add a"|} >:: (fun _ -> 
      assert_equal (parse "in abc add a") (In ("abc", Add ["a"])));
  {|Parse "in abc add a b"|} >:: (fun _ -> 
      assert_equal (parse "in abc add a b") (In ("abc", Add ["a";"b"])));
  {|Parse "in abc add"|} >:: (fun _ -> 
      let c = fun () -> parse "in abc add" in
      assert_raises (Malformed {|"in abc add"|}) c);
  {|Parse "in abc delete a b"|} >:: (fun _ -> 
      assert_equal (parse "in abc delete a b") (In ("abc", Delete ["a";"b"])));
  {|Parse "in abc delete a"|} >:: (fun _ -> 
      assert_equal (parse "in abc delete a") (In ("abc", Delete ["a"])));
  {|Parse "in abc delete"|} >:: (fun _ -> 
      let c = fun () -> parse "in abc delete" in
      assert_raises (Malformed {|"in abc delete"|}) c);
  {|Parse "in abc update 0 a a"|} >:: (fun _ -> 
      assert_equal (parse "in abc update 0 a a") 
        (In ("abc", Update {key=0; col="a"; value="a"})));
  {|Parse "in abc update 0 a"|} >:: (fun _ -> 
      let c = fun () -> parse "in abc update" in
      assert_raises (Malformed {|"in abc update"|}) c);
  {|Parse "in abc update 0"|} >:: (fun _ -> 
      let c = fun () -> parse "in abc update 0" in
      assert_raises (Malformed {|"in abc update 0"|}) c);
  {|Parse "in abc sum a b"|} >:: (fun _ -> 
      let c = fun () -> parse "in abc sum a b" in
      assert_raises (Malformed {|"in abc sum a b"|}) c);
  {|Parse "in abc sum a"|} >:: (fun _ -> 
      assert_equal (parse "in abc sum a") (In ("abc", Sum "a")));
  {|Parse "in abc sum"|} >:: (fun _ -> 
      let c = fun () -> parse "in abc sum" in
      assert_raises (Malformed {|"in abc sum"|}) c);
  {|Parse "in abc count a b"|} >:: (fun _ -> 
      let c = fun () -> parse "in abc count a b" in
      assert_raises (Malformed {|"in abc count a b"|}) c);
  {|Parse "in abc count a"|} >:: (fun _ -> 
      assert_equal (parse "in abc count a") (In ("abc", Count "a")));
  {|Parse "in abc count"|} >:: (fun _ -> 
      let c = fun () -> parse "in abc count" in
      assert_raises (Malformed {|"in abc count"|}) c);
  {|Parse "in abc count_null a b"|} >:: (fun _ -> 
      let c = fun () -> parse "in abc count_null a b" in
      assert_raises (Malformed {|"in abc count_null a b"|}) c);
  {|Parse "in abc count_null a"|} >:: (fun _ -> 
      assert_equal (parse "in abc count_null a") (In ("abc", CountNull "a")));
  {|Parse "in abc count_null"|} >:: (fun _ -> 
      let c = fun () -> parse "in abc count_null" in
      assert_raises (Malformed {|"in abc count_null"|}) c);

  (* General Errors *)
  {|Parse "in"|} >:: (fun _ -> 
      let c = fun () -> parse "in" in
      assert_raises (Malformed {|"in"|}) c);
  {|Parse "asdf" (malformed)|} >:: (fun _ -> 
      let c = fun () -> parse "asdf" in
      assert_raises (Malformed {|"asdf"|}) c);
  {|Parse "" (empty)|} >:: (fun _ -> 
      let c = fun () -> parse "" in
      assert_raises Empty c);
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

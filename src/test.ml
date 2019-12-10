open OUnit2

(** [f3 f a b t] is a simple composition function that calls [f] on [t] [a] [b],
    in that order. *)
let f3 f a b t = f t a b
(** [f2 f a t] is a simple composition function that calls [f] on [t] [a],
    in that order. *)
let f2 f a t = f t a

open Database
let database_tests =  [
  "create drop table test" >:: (fun _ -> 
      (try ignore (create_table "test" ["t";"e";"st"]); ()
       with _ -> failwith "create");
      (try ignore (drop_table "test"); ()
       with _ -> failwith "drop");
    );
  "fail create test" >:: (fun _ -> 
      let file = "test" in
      assert_raises (TableExists file) 
        (fun _ -> 
           ignore (create_table file ["t";"e";"st"]);
           ignore (create_table file ["t";"e";"st"]););
      ignore (drop_table file);
    );
  "fail drop test" >:: (fun _ -> 
      let file = "nonexistent" in
      assert_raises (TableNotFound file) 
        (fun _ -> 
           ignore (drop_table file));
    );
  "read write test" >:: (fun _ -> 
      let file = "test" in
      try
        ignore (create_table file ["t";"e";"st"]);
        assert_equal (write file (read file)) "key,t,e,st";
        ignore (drop_table file);
      with _ -> failwith "read write"
    );
]

open Row
open Command
let row_empty =          empty
let row_a =              empty |> f3 (add_column) "a" "a"
let row_b =              empty |> f3 (add_column) "b" "b"  
let row_a_update_b =     empty |> f3 (add_column) "a" "b" 
let row_ab =             empty 
                         |> f3 (add_column) "a" "a"
                         |> f3 (add_column) "b" "b"
let cond_row_a =         condition row_a ["a"]
let cond_row_ab_a =      condition row_ab ["a"]
let cond_row_ab_ab =     condition row_ab ["a";"b"]
let cond_row_ab_b =      condition row_ab ["b"]
let conds_a_lt_b_t =     [("a", LT, "b")]
let conds_a_lt_b_f =     [("a", LT, "0")]
let conds_a_lte_b_t =    [("a", LTE, "b")]
let conds_a_lte_b_t_eq = [("a", LTE, "a")]
let conds_a_lte_b_f =    [("a", LTE,"0")]
let conds_a_eq_a_t =     [("a", EQ, "a")]
let conds_a_eq_b_f =     [("a", EQ, "b")]
let conds_a_ne_b_t =     [("a", NE, "b")]
let conds_a_ne_a_f =     [("a", NE, "a")]
let conds_a_gt_b_f =     [("a", GT, "b")]
let conds_a_gt_b_t =     [("a", GT, "0")]
let conds_a_gte_b_f =    [("a", GTE, "b")]
let conds_a_gte_b_t_eq = [("a", GTE, "a")]
let conds_a_gte_b_t =    [("a", GTE,"0")]
let conds_mult_t_t =     [("a", LT, "b"); ("a", EQ, "a")]
let conds_mult_t_f =     [("a", LT, "b"); ("a", EQ, "b")]
let conds_mult_f_f =     [("a", LT, "0"); ("a", EQ, "b")]
let conds_mult_t_t_d =   [("a", EQ, "a"); ("b", EQ, "b")]
let row_tests = [
  "value: col a in row_a is a" >:: (fun _ ->
      assert_equal (value row_a "a") "a";);
  "value: col a in row_ab is a" >:: (fun _ ->
      assert_equal (value row_ab "b") "b");
  "adding col: adding column b in row_a is row_ab" >:: (fun _ ->
      assert_equal (add_column row_a "b" "b") row_ab;);
  "deleting col: column b in row_ab is row_a" >:: (fun _ ->
      assert_equal (delete_column row_ab "b") row_a;);
  "deleting col: column a in row_a is row_empty" >:: (fun _ ->
      assert_equal (delete_column row_a "a") row_empty;);
  "updating val: column a in row_a is b" >:: (fun _ ->
      assert_equal (update row_a "a" "b") row_a_update_b;);
  "building row: columns a b with values a b is row_ab" >:: (fun _ ->
      assert_equal (build_row ["a";"b"] ["a";"b"]) row_ab;);
  "to_csv row one val: to_csv of row_a" >:: (fun _ ->
      assert_equal (to_csv row_a) "a";);
  "to_csv row mult vals: to_csv of row_ab" >:: (fun _ ->
      assert_equal (to_csv row_ab) "a,b";);
  "to_csv row no vals: to_csv of row_a" >:: (fun _ ->
      assert_equal (to_csv row_empty) "";);
  "condition: LT, true, one cond; row_a, a < b" >:: (fun _ ->
      assert_equal (Some row_a) (cond_row_a conds_a_lt_b_t););
  "condition: LT, false, one cond; row_a, a < 0" >:: (fun _ ->
      assert_equal None (cond_row_a conds_a_lt_b_f););
  "condition: LTE, true, one cond; row_a, a <= b (less than)" >:: (fun _ ->
      assert_equal (Some row_a) (cond_row_a conds_a_lte_b_t););
  "condition: LTE, true, one cond; row_a, a <= a (equal)" >:: (fun _ ->
      assert_equal (Some row_a) (cond_row_a conds_a_lte_b_t_eq););
  "condition: LTE, false, one cond; row_a, a <= 0" >:: (fun _ ->
      assert_equal None (cond_row_a conds_a_lte_b_f););
  "condition: EQ, true, one cond; row_a, a = a" >:: (fun _ ->
      assert_equal (Some row_a) (cond_row_a conds_a_eq_a_t););
  "condition: EQ, false, one cond; row_a, a = b" >:: (fun _ ->
      assert_equal None (cond_row_a conds_a_eq_b_f););
  "condition: NE, true, one cond; row_a, a <> b" >:: (fun _ ->
      assert_equal (Some row_a) (cond_row_a conds_a_ne_b_t););
  "condition: NE, false, one cond; row_a, a <> a" >:: (fun _ ->
      assert_equal None (cond_row_a conds_a_ne_a_f););
  "condition: GT, true, one cond; row_a, a > 0" >:: (fun _ ->
      assert_equal (Some row_a) (cond_row_a conds_a_gt_b_t););
  "condition: GT, false, one cond; row_a, a > b" >:: (fun _ ->
      assert_equal None (cond_row_a conds_a_gt_b_f););
  "condition: GTE, true, one cond; row_a, a >= 0 (greater than)" >:: (fun _ ->
      assert_equal (Some row_a) (cond_row_a conds_a_gte_b_t););
  "condition: GTE, true, one cond; row_a, a >= a (equal)" >:: (fun _ ->
      assert_equal (Some row_a) (cond_row_a conds_a_gte_b_t_eq););
  "condition: GTE, false, one cond; row_a, a >= b" >:: (fun _ ->
      assert_equal None (cond_row_a conds_a_gte_b_f););
  "conditions: LT - true, EQ - true, two conds; a < b, a = a" >:: (fun _ ->
      assert_equal (Some row_a) (cond_row_a conds_mult_t_t););
  "conditions: LT - true, EQ - false, two conds; a < b, a = b" >:: (fun _ ->
      assert_equal None (cond_row_a conds_mult_t_f););
  "conditions: LT - false, EQ - false, two conds; a < 0, a = b" >:: (fun _ ->
      assert_equal None (cond_row_a conds_mult_f_f););
  "condition: No conditions, select a from a." >:: (fun _ ->
      assert_equal (Some row_a) (cond_row_a []););
  "condition: No conditions, select a from ab." >:: (fun _ ->
      assert_equal (Some row_a) (cond_row_ab_a []););
  "condition: select one col, condition another" >:: (fun _ ->
      assert_equal (Some row_b) (cond_row_ab_b conds_a_eq_a_t);); 
  "condition: select two col, condition both" >:: (fun _ ->
      assert_equal (Some row_ab) (cond_row_ab_ab conds_mult_t_t_d);); 
  "condition: select two col, condition one" >:: (fun _ ->
      assert_equal (Some row_ab) (cond_row_ab_ab conds_a_eq_a_t);); 
  "condition: conditioning invalid column in condition." >:: (fun _ ->
      let inval_call = fun () -> cond_row_a [("c", EQ,"c")] in
      assert_raises (InvalidColumn "c") inval_call;);
  "condition: Invalid column in column selection, w/ conditions" >:: (fun _ ->
      let inval_call = fun () -> (condition row_a ["d"]) conds_a_eq_a_t in
      assert_raises (InvalidColumn "d") inval_call;);
  "condition: Invalid column in column selection, no conditions" >:: (fun _ ->
      let inval_call = fun () -> (condition row_a ["d"]) [] in
      assert_raises (InvalidColumn "d") inval_call;);
]

open Table
let table_empty = empty
let table_ab_nocols_1 = empty |> f2 insert_row row_ab
let table_ab_cols_1 = table_ab_nocols_1 |> f2 add_columns ["a";"b"]
let row0 = Row.empty 
           |> f3 (Row.add_column) "a" "0a"
           |> f3 (Row.add_column) "b" "0b"
           |> f3 (Row.add_column) "c" "0c"
let row0alt = Row.empty 
              |> f3 (Row.add_column) "a" "0a"
              |> f3 (Row.add_column) "b" "0b"
              |> f3 (Row.add_column) "c" "alt"
let row0noc = Row.empty 
              |> f3 (Row.add_column) "a" "0a"
              |> f3 (Row.add_column) "b" "0b"
let row1 = Row.empty 
           |> f3 (Row.add_column) "a" "1a"
           |> f3 (Row.add_column) "b" "1b"
           |> f3 (Row.add_column) "c" "1c"
let row2 = Row.empty 
           |> f3 (Row.add_column) "a" "2a"
           |> f3 (Row.add_column) "b" "2b"
           |> f3 (Row.add_column) "c" "2c"
let table_abc_cols_3 = empty |> f2 add_columns ["a";"b";"c"]
                       |> f2 insert_row row0 
                       |> f2 insert_row row1 
                       |> f2 insert_row row2
let table_abc_cols_2 = empty |> f2 add_columns ["a";"b";"c"]
                       |> f2 insert_row row0 
                       |> f2 insert_row row1
let table_abc_cols_1 = empty |> f2 add_columns ["a";"b";"c"]
                       |> f2 insert_row row0 
let table_abc_cols_1_alt = empty |> f2 add_columns ["a";"b";"c"]
                           |> f2 insert_row row0alt 
let table_abc_cols_1_nc = empty |> f2 add_columns ["a";"b"]
                          |> f2 insert_row row0noc 
let numr0 = Row.empty |> f3 Row.add_column "a" "1"
let table_num = empty |> f2 add_columns ["a"] 
                |> f2 insert_row numr0 (* 1 *)
                |> f2 insert_row numr0 (* 2 *)
                |> f2 insert_row numr0 (* 3 *)
                |> f2 insert_row numr0 (* 4 *)
                |> f2 insert_row numr0 (* 5 *)
let table_tests = [
  "set_columns and get_column_names: set empty columns to a, b, c" >:: (fun _ -> 
      assert_equal ["a";"b";"c"] (set_columns table_empty ["a";"b";"c"] 
                                  |> get_column_names););
  "read_insert_row: row_ab to empty" >:: (fun _ -> 
      assert_equal table_ab_nocols_1 
        (read_insert_row empty 0 row_ab));      
  "insert_row: row2 insert in abc_cols_2 is abc_cols_3" >:: (fun _ -> 
      assert_equal table_abc_cols_3 
        (insert_row table_abc_cols_2 row2));   
  (* BROKEN TEST CASE 
       "remove_rows: removing rows 2, 1 from abc_cols_3 is abc_cols_1" >:: (fun _ -> 
        assert_equal table_abc_cols_1 (remove_rows table_abc_cols_3 [2;1]));  *)  
  "get_column: column a in abc_cols_1 is [(0,0a)]" >:: (fun _ -> 
      assert_equal [(0,"0a")] (get_column table_abc_cols_1 "a"));
  "get_column: column b in abc_cols_2 is [(0,0b);(1,1b)]" >:: (fun _ -> 
      assert_equal [(0,"0b");(1,"1b")] (get_column table_abc_cols_2 "b"));
  "get_column: e in abc_cols_1, raises InvalidColumn e" >:: (fun _ ->
      let c = fun () -> get_column table_abc_cols_1 "e" in 
      assert_raises (InvalidColumn "e") c);    
  "update_cell: " >:: (fun _ -> 
      assert_equal table_abc_cols_1_alt 
        (update_cell table_abc_cols_1 0 "c" "alt"));
  "update_cell: 1 in abc_cols_1, raises InvalidKey 1" >:: (fun _ ->
      let c = fun () -> update_cell table_abc_cols_1 1 "c" "alt" in 
      assert_raises (InvalidKey "1") c);
  "select: columns a b in abc_cols_1 no conditions" >:: (fun _ -> 
      assert_equal table_abc_cols_1_nc 
        (select ["a";"b";] [] table_abc_cols_1));
  (* BROKEN TEST CASES
     "select: columns a b in abc_cols_2 when a = a0" >:: (fun _ -> 
        assert_equal table_abc_cols_1_nc 
          (select ["a";"b";] [("a",EQ,"a0")] table_abc_cols_1));
       "select: cells in abc_cols_3 less than a2 is abc_cols_2" >:: (fun _ -> 
        assert_equal table_abc_cols_2 
          (select ["a";"b";"c"] [("a",LT,"a2")] table_abc_cols_3));   *) 
  "select: invalid column d in selection, raises InvalidColumn d" >:: (fun _ -> 
      let c = fun () -> (select ["d"] [] table_abc_cols_1) in
      assert_raises (InvalidColumn "d") c);
  "select: invalid column d in condition, raises InvalidColumn d" >:: (fun _ -> 
      let c = fun () -> 
        (select ["a"] [("d",LT,"a2")] table_abc_cols_1) 
      in assert_raises (InvalidColumn "d") c);
  (* BROKEN TEST CASES
     "select_all: abc_cols_3 no conditions" >:: (fun _ -> 
        assert_equal table_abc_cols_3 (select_all [] table_abc_cols_3));
       "select_all: abc_cols_3 when a = a0" >:: (fun _ -> 
        assert_equal table_abc_cols_1 
          (select_all [("a",EQ,"a0")] table_abc_cols_3)); *)   
  (* ADDING/REMOVING COLUMNS -> check table as well? *)
  "add_columns: one column to abc_cols_3" >:: (fun _ -> 
      assert_equal (["a";"b";"c";"d"]) 
        (table_abc_cols_3 
         |> f2 add_columns ["d"] |> get_column_names)); 
  "add_columns: two columns to abc_cols_3" >:: (fun _ -> 
      assert_equal (["a";"b";"c";"d";"e"]) 
        (table_abc_cols_3 
         |> f2 add_columns ["d";"e"] |> get_column_names));  
  "delete_columns: deleting one column from abc_cols_3" >:: (fun _ -> 
      assert_equal (["a";"b"]) 
        (table_abc_cols_3 
         |> f2 delete_columns ["c"] |> get_column_names));       
  "delete_columns: deleting two columns from abc_cols_3" >:: (fun _ -> 
      assert_equal (["a"]) 
        (table_abc_cols_3 
         |> f2 delete_columns ["b";"c"] |> get_column_names));   
  "sum_column: sum is 5" >:: (fun _ -> 
      assert_equal (sum_column table_num "a") "5"); 
  "sum_column: type error" >:: (fun _ -> 
      let c = fun () -> (sum_column table_abc_cols_3 "a") in
      assert_raises TypeError c);   
  "count: count empty is 0" >:: (fun _ -> 
      assert_equal 
        (count (empty |> f2 add_columns ["a"]) "a") "0"); 
  "count: count is 3" >:: (fun _ -> 
      assert_equal (count table_abc_cols_3 "a") "3");   
  "count: count is 1" >:: (fun _ -> 
      assert_equal (count table_abc_cols_1 "a") "1");   
  (* BROKEN TEST CASE
     "count_null: null values from new column" >:: (fun _ ->  
      assert_equal "3"
        (add_columns table_abc_cols_3 ["d"] |> f2 count_null "d"));  *)  
  "count_null: no null values in existing column" >:: (fun _ ->  
      assert_equal "0" (count_null table_abc_cols_3 "a"));                        
  "to_csv test" >:: (fun _ -> 
      let row_a = Row.add_column (Row.empty) "col" "a" in
      let row_b = Row.add_column (Row.empty) "col" "b" in
      let t = insert_row (add_columns empty ["col"]) row_a in
      assert_equal (to_csv t) "key,col\n0,a";
      assert_equal 
        (to_csv (insert_row t row_b)) "key,col\n0,a\n1,b";);
]

open Log
let log_tests = [
  "write to log and get log" >:: (fun _ -> 
      ignore (write_log "test1");
      assert_equal "test1" (get_log ()););
  "write more log and get log" >:: (fun _ -> 
      ignore (write_log "test2");
      assert_equal "test1\ntest2" (get_log ()););
  "clear log" >:: (fun _ ->
      ignore (clear ());
      assert_raises (Sys_error "log.txt: No such file or directory") 
        (fun _ -> get_log ()));
]

let command_tests = [
  (* General Commands *)
  {|Parse "log"|} >:: (fun _  -> assert_equal Log (parse "log"));
  {|Parse "undo"|} >:: (fun _ -> assert_equal Undo (parse "undo"));
  {|Parse "quit"|} >:: (fun _ -> assert_equal Quit (parse "quit"));
  {|Parse "help"|} >:: (fun _ -> assert_equal Help (parse "help"));
  {|Parse "create abc a b c"|} >:: (fun _ -> 
      assert_equal (Create {file="abc"; cols=["a";"b";"c"]})
        (parse "create abc a b c"));
  {|Parse "create"|} >:: (fun _ -> 
      let c = fun () -> parse "create" in 
      assert_raises (Malformed {|"create"|}) c);
  {|Parse "create abc"|} >:: (fun _ -> 
      let c = fun () -> parse "create abc" in
      assert_raises (Malformed {|"create abc"|}) c);
  {|Parse "drop abc"|} >:: (fun _ -> 
      assert_equal (Drop "abc") (parse "drop abc"));
  {|Parse "drop"|} >:: (fun _ -> 
      let c = fun () -> parse "drop" in
      assert_raises (Malformed {|"drop"|}) c);
  (* Table Commands *)
  {|Parse "in abc select a where a > 0"|} >:: (fun _ -> 
      assert_equal (In ("abc", Select (["a"],[("a",GT,"0")])))
        (parse "in abc select a where a > 0"));
  {|Parse "in abc select a where"|} >:: (fun _ -> 
      assert_equal (In ("abc", Select (["a"],[])))
        (parse "in abc select a where"));
  {|Parse "in abc select a b where a > 0"|} >:: (fun _ -> 
      assert_equal (In ("abc", Select (["a";"b"],[("a",GT,"0")])))
        (parse "in abc select a b where a > 0"));
  {|Parse "in abc select a where a > 0 & a = 1"|} >:: (fun _ -> 
      assert_equal (In ("abc", Select (["a"],[("a",EQ,"1");("a",GT,"0")])))
        (parse "in abc select a where a > 0 & a = 1"));
  {|Parse "in abc select a"|} >:: (fun _ -> 
      assert_equal (In ("abc", Select (["a"],[])))
        (parse "in abc select a") );
  {|Parse "in abc select a b"|} >:: (fun _ -> 
      assert_equal (In ("abc", Select (["a";"b"],[])))
        (parse "in abc select a b"));
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
      assert_equal (In ("abc", SelectStar [])) (parse "in abc select *"));
  {|Parse "in abc select * where a > 0"|} >:: (fun _ -> 
      assert_equal  (In ("abc", SelectStar [("a",GT,"0")]))
        (parse "in abc select * where a > 0"));
  {|Parse "in abc select * where"|} >:: (fun _ -> 
      assert_equal (In ("abc", SelectStar [])) (parse "in abc select * where"));
  {|Parse "in abc insert a b c"|} >:: (fun _ -> 
      assert_equal (In ("abc", Insert ["a";"b";"c"])) 
        (parse "in abc insert a b c"));
  {|Parse "in abc insert"|} >:: (fun _ -> 
      let c = fun () -> parse "in abc select" in
      assert_raises (Malformed {|"in abc select"|}) c); 
  {|Parse "in abc remove 0"|} >:: (fun _ -> 
      assert_equal (In ("abc", Remove [0])) (parse "in abc remove 0"));
  {|Parse "in abc remove 0 1"|} >:: (fun _ -> 
      assert_equal (In ("abc", Remove [1;0])) (parse "in abc remove 0 1"));
  {|Parse "in abc remove"|} >:: (fun _ -> 
      let c = fun () -> parse "in abc remove" in
      assert_raises (Malformed {|"in abc remove"|}) c);
  {|Parse "in abc add a"|} >:: (fun _ -> 
      assert_equal (In ("abc", Add ["a"])) (parse "in abc add a"));
  {|Parse "in abc add a b"|} >:: (fun _ -> 
      assert_equal (In ("abc", Add ["a";"b"])) (parse "in abc add a b"));
  {|Parse "in abc add"|} >:: (fun _ -> 
      let c = fun () -> parse "in abc add" in
      assert_raises (Malformed {|"in abc add"|}) c);
  {|Parse "in abc delete a b"|} >:: (fun _ -> 
      assert_equal (In ("abc", Delete ["a";"b"])) (parse "in abc delete a b"));
  {|Parse "in abc delete a"|} >:: (fun _ -> 
      assert_equal (In ("abc", Delete ["a"])) (parse "in abc delete a"));
  {|Parse "in abc delete"|} >:: (fun _ -> 
      let c = fun () -> parse "in abc delete" in
      assert_raises (Malformed {|"in abc delete"|}) c);
  {|Parse "in abc update 0 a a"|} >:: (fun _ -> 
      assert_equal (In ("abc", Update {key=0; col="a"; value="a"}))
        (parse "in abc update 0 a a"));
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
      assert_equal (In ("abc", Sum "a")) (parse "in abc sum a"));
  {|Parse "in abc sum"|} >:: (fun _ -> 
      let c = fun () -> parse "in abc sum" in
      assert_raises (Malformed {|"in abc sum"|}) c);
  {|Parse "in abc count a b"|} >:: (fun _ -> 
      let c = fun () -> parse "in abc count a b" in
      assert_raises (Malformed {|"in abc count a b"|}) c);
  {|Parse "in abc count a"|} >:: (fun _ -> 
      assert_equal (In ("abc", Count "a")) (parse "in abc count a"));
  {|Parse "in abc count"|} >:: (fun _ -> 
      let c = fun () -> parse "in abc count" in
      assert_raises (Malformed {|"in abc count"|}) c);
  {|Parse "in abc count_null a b"|} >:: (fun _ -> 
      let c = fun () -> parse "in abc count_null a b" in
      assert_raises (Malformed {|"in abc count_null a b"|}) c);
  {|Parse "in abc count_null a"|} >:: (fun _ -> 
      assert_equal (In ("abc", CountNull "a")) (parse "in abc count_null a"));
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

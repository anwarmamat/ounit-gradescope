open OUnit2
open Basics

let test_part1 ctxt =

  assert_equal (4, 5, 6) (rev_tup (6, 5, 4)) ~msg:"part1 (1)";

  assert_equal 3 (max_tup (3, 2, 1)) ~msg:"part1 (2)";
  assert_equal 3 (max_tup (1, 3, 3)) ~msg:"part1 (3)";
  assert_equal 3 (max_tup (3, 3, 1)) ~msg:"part1 (4)";
  assert_equal 3 (max_tup (3, 1, 3)) ~msg:"part1 (5)";
  assert_equal 3 (max_tup (1, 1, 3)) ~msg:"part1 (6)";
  assert_equal 3 (max_tup (3, 1, 1)) ~msg:"part1 (7)";
  assert_equal 3 (max_tup (1, 3, 1)) ~msg:"part1 (8)";
  assert_equal 3 (max_tup (3, 3, 3)) ~msg:"part1 (9)";
  assert_equal 3 (max_tup (1, 2, 3)) ~msg:"part1 (10)";

  assert_equal 9 (area (1, 1) (4, 4)) ~msg:"part1 (11)";

  assert_equal 27 (volume (1, 1, 1) (4, 4, 4)) ~msg:"part1 (12)";

  assert_equal false (equiv_frac (21, 30) (4, 6)) ~msg:"part1 (13)";
  assert_equal true (equiv_frac (18, 27) (4, 6)) ~msg:"part1 (14)"


let test_abs ctxt = assert_equal 0 (abs 0) ~msg:"abs (1)"

let test_area_volume ctxt =

  assert_equal 1 (area (2, 2) (1, 1)) ~msg:"area_volume (1)";
  assert_equal 2 (area (2, 1) (1, 3)) ~msg:"area_volume (2)";
  assert_equal 2 (area (3, 1) (1, 2)) ~msg:"area_volume (3)";
  assert_equal 4 (area (3, 1) (1, 3)) ~msg:"area_volume (4)";
  assert_equal 0 (area (0, 0) (0,0)) ~msg:"area_volume (5)";

  assert_equal 1 (volume (2, 2, 2) (1, 1, 1)) ~msg:"area_volume (6)";
  assert_equal 4 (volume (2, 3, 3) (1, 1, 1)) ~msg:"area_volume (7)";
  assert_equal 4 (volume (3, 2, 3) (1, 1, 1)) ~msg:"area_volume (8)";
  assert_equal 4 (volume (3, 3, 2) (1, 1, 1)) ~msg:"area_volume (9)";
  assert_equal 8 (volume (3, 3, 3) (1, 1, 1)) ~msg:"area_volume (10)";
  assert_equal 0 (volume (3, 3, 3) (3, 3, 3)) ~msg:"area_volume (11)"

let test_equiv_frac ctxt =

  assert_equal false (equiv_frac (0, 30) (20, 0)) ~msg:"equiv_frac (1)";
  assert_equal true (equiv_frac (0, 30) (0, 20)) ~msg:"equiv_frac (2)";
  assert_equal false (equiv_frac (30, 0) (0, 20)) ~msg:"equiv_frac (3)";
  assert_equal false (equiv_frac (30, 0) (20, 0)) ~msg:"equiv_frac (4)"

let test_part2 ctxt =

  assert_equal 720 (factorial 6) ~msg:"part2 (1)";

  assert_equal 5 (pow 5 1) ~msg:"part2 (2)";
  assert_equal 36 (pow 6 2) ~msg:"part2 (3)";

  assert_equal 1 (tail 1 1) ~msg:"part2 (4)";
  assert_equal 35 (tail 8935 2) ~msg:"part2 (4)";

  assert_equal 1 (log 2 3) ~msg:"part2 (6)";
  assert_equal 1 (log 2 2) ~msg:"part2 (7)";
  assert_equal 3 (log 10 1000) ~msg:"part2 (8)";

  assert_equal 1 (len 4) ~msg:"part2 (9)";
  assert_equal 2 (len 10) ~msg:"part2 (10) ";
  assert_equal 5 (len 12355) ~msg:"part2 (11)";

  assert_equal false (contains 22224 2224) ~msg:"part2 (12)";
  assert_equal true (contains 330 1133011) ~msg:"part2 (13)";
  assert_equal true (contains 100 31100203) ~msg:"part2 (14)"


let test_factorial ctxt = assert_equal 1 (factorial 0) ~msg:"factorial (1)"

let test_pow ctxt =
  assert_equal 1 (pow 1 0) ~msg:"pow (1)";
  assert_equal 1 (pow (-5) 0) ~msg:"pow (2)";
  assert_equal 1 (pow 5 0) ~msg:"pow (3)"

let test_tail ctxt =

  assert_equal 125 (tail 125 4) ~msg:"tail (1)";
  assert_equal 0 (tail 125 0) ~msg:"tail (2)"

let test_log ctxt = assert_equal 0 (log 2 1) ~msg:"log (1)"

let test_contains ctxt =

  assert_equal false (contains 312331 0) ~msg:"contains (1)";
  assert_equal false (contains 0 312331) ~msg:"contains (2)"

let test_len ctxt =

  assert_equal 1 (len 0) ~msg:"len (1)";
  assert_equal 4 (len (-1234)) ~msg:"len (2)"

let test_var ctxt =
  let t = push_scope (empty_table ()) in
  let t = add_var "x" 3 t in
  let t = add_var "y" 4 t in
  let t = add_var "asdf" 14 t in
  let t = add_var "x" 5 t in
  assert_equal 5 (lookup "x" t) ~msg:"test_var (1)"

let test_scopes ctxt =
  let a1 = add_var "a" 1 (push_scope (empty_table ())) in
  let a2 = add_var "a" 2 (push_scope a1) in
  let a3 = add_var "a" 3 (push_scope a2) in
  let a4 = add_var "a" 4 (push_scope a3) in
  let a5 = add_var "a" 5 (push_scope a4) in
  let a6 = add_var "a" 6 (push_scope a5) in
  let a5 = pop_scope a6 in
  let a4 = pop_scope a5 in
  let a3 = pop_scope a4 in
  let a2 = pop_scope a3 in
  let a1 = pop_scope a2 in
  let a0 = pop_scope a1 in
  assert_equal 1 (lookup "a" a1) ~msg:"test_scopes (1)";
  assert_equal 2 (lookup "a" a2) ~msg:"test_scopes (2)";
  assert_equal 3 (lookup "a" a3) ~msg:"test_scopes (3)";
  assert_equal 4 (lookup "a" a4) ~msg:"test_scopes (4)";
  assert_equal 5 (lookup "a" a5) ~msg:"test_scopes (5)";
  assert_equal 6 (lookup "a" a6) ~msg:"test_scopes (6)";
  assert_raises (Failure "No scopes remain!") (fun () -> pop_scope a0) ~msg:"test_scopes (7)";
  assert_raises (Failure "Variable not found!") (fun () -> lookup "a" a0) ~msg:"test_scopes (8)"

let suite =
  "release" >::: [
    "part1" >:: test_part1;
    "abs" >:: test_abs;
    "area_volume" >:: test_area_volume;
    "equiv_frac" >:: test_equiv_frac;
    "part2" >:: test_part2;
    "factorial" >:: test_factorial;
    "pow" >:: test_pow;
    "tail" >:: test_tail;
    "log" >:: test_log;
    "contains" >:: test_contains;
    "len" >:: test_len;
    "var" >:: test_var;
    "scopes" >:: test_scopes
  ]

let _ = run_test_tt_main suite

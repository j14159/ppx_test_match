open OUnit2

type test_rec = { x : int; y : float }

let test_exact _ =
  [%test_match? ("hello", 2)] ("hello", 2);
  [%test_match? "some string"] "some string";
  [%test_match? { x = 1; y = 1.2 }] { x = 1; y = 1.2 }

let test_records _ =
  let rec_a = { x = 1; y = 2.5 } in
  [%test_match? { x = 1; _ }] rec_a;
  [%test_match? { x = 1; y } when y > 2.1 && y < 3.0] rec_a

let test_match_eq_tuples _ =
  let tup = (1, 1) in
  [%test_match? (x, x)] tup;
  [%test_match? ((x, y), x) when y > 5.0] (("abc", 5.1), "abc")

let suite =
  "Basic tests checking that test_match functions as expected" >:::
    [ "Exact match" >:: test_exact
    ; "Record matches" >:: test_records
    ; "Match equality in tuples" >:: test_match_eq_tuples
    ]

let _ = run_test_tt_main suite



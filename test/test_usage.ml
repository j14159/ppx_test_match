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

type test_rec2 = { a : int; b : string; c : int }

let test_match_eq_records _ =
  [%test_match? (x, { x; _ })] (1, { x = 1; y = 2.0 });
  [%test_match? { a; c = a; _ }] { a = 1; b = "abc"; c = 1 }

let test_failed_eq _ =
  let expected = Failure "No match" in
  assert_raises expected (fun _ -> [%test_match? (x, x)] (1, 2));
  assert_raises expected (fun _ -> [%test_match? {y; _} when y < 1.0] { x = 1; y = 2.0 });
  assert_raises expected (fun _ -> [%test_match? (x, x) when x > 5] (4, 4))

let suite =
  "Basic tests checking that test_match functions as expected" >:::
    [ "Exact match" >:: test_exact
    ; "Record matches" >:: test_records
    ; "Match equality in tuples" >:: test_match_eq_tuples
    ; "Match equality in records" >:: test_match_eq_records
    ; "Fail equality checks" >:: test_failed_eq
    ]

let _ = run_test_tt_main suite



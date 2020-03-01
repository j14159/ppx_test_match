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
  assert_raises
    (Failure "Failed to match (x, x)")
    (fun _ -> [%test_match? (x, x)] (1, 2));
  assert_raises
    (* Formatting seems a bit odd but I won't argue with ppxlib's pretty
       printer for now.
     *)
    (Failure "Failed to match { y;_} when y < 1.0")
    (fun _ -> [%test_match? {y; _} when y < 1.0] { x = 1; y = 2.0 });
  assert_raises
    (Failure "Failed to match (x, x) when x > 5")
    (fun _ -> [%test_match? (x, x) when x > 5] (4, 4))

type 'a test_variant = A of 'a

let test_variants _ =
  [%test_match? (A a) when a > 5] (A 6);
  let m = [%test_match? (a, A a) when a > 1.2] in
  m (2.0, A 2.0);
  let expected = Failure "Failed to match (a, A a) when a > 1.2" in
  assert_raises expected (fun _ -> m (1.0, A 1.0));
  assert_raises expected (fun _ -> m (5.0, A 5.1))

let test_printer _ =
  assert_raises
    (Failure "Failed to match (x, x):  (1, 2)")
    (fun _ -> [%test_match? (x, x)] (1, 2) ~printer:[%derive.show:(int * int)]);
  assert_raises
    (Failure "Failed to match (\"x\", y) when y > 2.4:  (\"x\", 2.39)")
    (fun _ -> [%test_match? ("x", y) when y > 2.4]
                ("x", 2.39)
                ~printer:[%derive.show:(string * float)])

let suite =
  "Basic tests checking that test_match functions as expected" >:::
    [ "Exact match" >:: test_exact
    ; "Record matches" >:: test_records
    ; "Match equality in tuples" >:: test_match_eq_tuples
    ; "Match equality in records" >:: test_match_eq_records
    ; "Fail equality checks" >:: test_failed_eq
    ; "Variants, including equality." >:: test_variants
    ; "Using printer argument" >:: test_printer
    ]

let _ = run_test_tt_main suite



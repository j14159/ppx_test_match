ppx\_test\_match
=====
A PPX rewriter to allow simpler testing of pattern matches rather than full equality.  Roughly modelled on [EUnit's](http://erlang.org/doc/apps/eunit/chapter.html) `?assertMatch` macro.

**Warning:** this is still very early and incomplete.

Please note that this project is released with a Contributor Code of Conduct, version 1.4. By participating in this project you agree to abide by its terms.  Please see `code_of_conduct.md` in this repository for details.

# Usage
Add this preprocessing step to your `dune` file's library, executable, or test:

```
(preprocess (pps ppx_test_match?))
```

See `dune/test` for a more complete example or [the Dune documentation](https://dune.readthedocs.io/en/stable/concepts.html#preprocessing-spec).

This is still too early to be fully usable but the following are already possible:

```ocaml
let some_ounit_test _ =
  let example_tuple = (2, 3) in
  (* This will pass:  *)
  [%test_match? (x, y) when y > x] example_tuple;
  (* This will fail:  *)
  [%test_match? (x, y) when x > y] example_tuple;
  (* This will pass:  *)
  [%test_match? (2, _)] example_tuple

type some_rec = { x : string; y : float }

let some_other_test_with_records =
  let example_rec = { x = "hello"; y = 3.14159 } in
  (* This will pass:  *)
  [%test_match? { x = "hello"; _ }] example_rec

type a_variant = A of int

let test_with_variant _ =
  (* Will pass:  *)
  [%test_match? (A a) when a > 4] (A 5)
```

The PPX rewriter will also rewrite duplicated variables as an equality check, for example:

```ocaml
let some_other_ounit_test _ =
  let t1 = (1.3, 1.3) in
  let t2 = (1.0, 1.01) in
  (* This will pass:  *)
  [%test_match? (x, x)] t1;
  (* This will fail:  *)
  [%test_match? (x, x)] t2

type some_other_rec = { a : int; b : string; c : int }

let another_test _ =
  (* This will pass:  *)
  [%test_match? { a; c = a; _ }] { a = 1; b = "abc"; c = 1 }

type a_variant = A of int

let test_eq_with_variant _ =
  (* Will pass:  *)
  [%test_match? (a, A a)] (2, A 2)
  (* Will fail:  *)
  [%test_match? (a, A a) when a < 5] (6, A 6)
```

Only tuples, records, and variants support this rewriting at the moment.

## Printer Argument
You can optionally supply a `printer` argument to `test_match` that will be used to format a non-matching expression on failure.  An example using [ppx_deriving](https://github.com/ocaml-ppx/ppx_deriving) to generate the printer:

```ocaml
let test_with_printer _ =
  (* This will fail with the following message in a `Failure` exception:

       Failed to match (x, x) with value (1, 2)
   *)
  [%test_match? (x, x)] (1, 2) ~printer:[derive.show: (int * int)]
```

# Rewriting Details
When `test_match?` sees something like `(x, x)`, it will synthesize the following replacement:

```ocaml
(x, _syn_0x) when x = _syn_0x
```

A more complex example, e.g. `((x, y), x) when y > x)` will be rewritten as follows:

```ocaml
((x, y), _syn_0x) when y > x && x = _syn_0x
```

More occurrences of the same variable are sequentially named, e.g. `(a, a, a, b)` will be rewritten to:

```ocaml
(a, _syn_0a, _syn_1a, b)
```

# Licensing
ppx\_test\_match is released under the terms of the MIT license.  Please see `LICENSE.md` for more details.

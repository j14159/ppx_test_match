(* Tests specifically for the expected output/correctness of patterns that are
   rewritten to convert duplicate variable names to synthesized variables and
   equality checks.
 *)
open OUnit2
open Ppxlib
open Ast_builder.Default

(* Helper to make positions on line 1 of a no-name file (string lexbuf) for
   easier tests.
 *)
let no_file_pos pos_bol pos_cnum =
  { pos_fname = ""; pos_lnum = 1; pos_bol; pos_cnum }

(* Another small helper to make location creation simpler for single-line
   sources.
*)
let one_line_loc s e =
  location ~start:(no_file_pos 0 s) ~end_:(no_file_pos 0 e) ~ghost:false

(* Printers for error reporting.  *)
let pat_printer p =
  let f = Format.str_formatter in
  Pprintast.pattern f p;
  Format.flush_str_formatter ()

let exp_printer e =
  let f = Format.str_formatter in
  Pprintast.expression f e;
  Format.flush_str_formatter ()

(* Useful for debugging/exploration. *)
let pos_printer = function
  | { pos_fname; pos_lnum; pos_bol; pos_cnum } ->
     let x = (List.map string_of_int [pos_lnum; pos_bol; pos_cnum])
             |> List.fold_left (fun a n -> a ^ "; " ^ n) pos_fname
     in
     "(" ^ x ^ ")"

(* Useful for debugging/exploration. *)
let loc_printer = function
  | { loc_start = ls; loc_end = le; loc_ghost = lg } ->
      "[" ^ (pos_printer ls) ^ " " ^ (pos_printer le) ^ " " ^ (string_of_bool lg) ^ "]"

(* Ensure that a simple 2-tuple pattern that checks for equality gets correctly
   as two different variables with a correct synthetic guard.  In this case, we
   want `(x, x)` to be rewritten as follows:

     (x, _syn_0x) when x = _syn_0x
 *)
let test_basic_tuple_eq_rewrite _ =
  let text_pat = "(x, x)" in
  let pat = Parse.pattern (Lexing.from_string text_pat) in
  let (rewritten, synth_guard_opt) = Ppx_test_match.rewrite_patt pat None in
  let expected = ppat_tuple
                   ~loc:(location ~start:(no_file_pos 0 0) ~end_:(no_file_pos 0 6) ~ghost:false)
                   [ ppat_var
                       ~loc:(location ~start:(no_file_pos 0 1) ~end_:(no_file_pos 0 2) ~ghost:false)
                       { txt = "x"
                       ; loc = (location ~start:(no_file_pos 0 1) ~end_:(no_file_pos 0 2) ~ghost:false)
                       }
                   ; ppat_var
                       ~loc:(location ~start:(no_file_pos 0 4) ~end_:(no_file_pos 0 5) ~ghost:false)
                       { txt = "_syn_0x"
                       ; loc = (location ~start:(no_file_pos 0 4) ~end_:(no_file_pos 0 5) ~ghost:false)
                       }
                   ]
  in
  assert_equal expected rewritten ~printer:pat_printer;
  assert_bool "Synthetic guard exists." (Option.is_some synth_guard_opt);
  let synth_guard = Option.get synth_guard_opt in
  let loc = one_line_loc 1 2 in
  (* First argument shares its position with the original variable as does the
     entire guard.
   *)
  let arg2_loc = one_line_loc 4 5 in
  let expected_guard = pexp_apply
                         ~loc
                         (pexp_ident ~loc { txt = Lident "="; loc })
                         [ ( Nolabel,
                             pexp_ident
                               ~loc
                               { txt = Lident "x"; loc }
                           )
                         ; ( Nolabel,
                             pexp_ident
                               ~loc:arg2_loc
                               { txt = Lident "_syn_0x"; loc = arg2_loc }
                           )
                         ]
  in
  assert_equal expected_guard synth_guard ~printer:exp_printer

let suite =
  "Pattern rewrite tests" >:::
    [ "Simple 2-member tuple equality synthesis" >:: test_basic_tuple_eq_rewrite

    ]

let _ = run_test_tt_main suite
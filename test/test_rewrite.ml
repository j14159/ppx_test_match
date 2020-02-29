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
   rewritten as two different variables in a new pattern with a correct
   synthetic guard.  In this particular test case, we want `(x, x)` to be
   rewritten as follows:

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

(** Convenience function for comparing locations.  *)
let loc_equal = assert_equal ~printer:loc_printer

(* Check that the synthesized guard is added to the user-supplied one, rather than
   overwriting it.
 *)
let test_tuple_rewrite_with_user_guard _ =
  let pat_source = "(y, y)" in
  (* Offset to make it look like the position is that from `(y, y) when y > 2` *)
  let grd_source = (String.make (String.length (pat_source ^ " when ")) ' ') ^ "y > 2" in
  let pat = Parse.pattern (Lexing.from_string pat_source) in
  let grd = Parse.expression (Lexing.from_string grd_source) in
  let (_rewritten, full_guard_opt) = Ppx_test_match.rewrite_patt pat (Some grd) in
  assert_bool "Guard exists" (Option.is_some full_guard_opt);
  let full_guard = Option.get full_guard_opt in

  (* The user-supplied guard's location should be used for all guards, which
     here means for the application of `&&` to the user-supplied guard and
     synthesized guards.
   *)
  let full_guard_exp_loc = one_line_loc 12 17 in
  loc_equal full_guard_exp_loc full_guard.pexp_loc;

  match full_guard with
  | { pexp_desc = Pexp_apply (exp, args); _ } ->
     let exp_loc = (one_line_loc 12 17) in
     let expected_exp = pexp_ident ~loc:exp_loc { txt = Lident "&&"; loc = exp_loc } in
     loc_equal expected_exp.pexp_loc exp.pexp_loc;
     assert_equal expected_exp exp ~printer:exp_printer;
     (* Check correctness of locations:  *)
     begin
       match args with
       | [(Nolabel, user_guard); (Nolabel, synth_guard)] ->
          (* User-supplied guard should have the expected location:  *)
          loc_equal user_guard.pexp_loc full_guard_exp_loc;
          (* Synthetic guard should have the same location as the first `y`:  *)
          loc_equal (one_line_loc 1 2) synth_guard.pexp_loc;

          (* Check correctness of generated guard:  *)
          begin
            let base_loc = one_line_loc 1 2 in
            let synth_loc = one_line_loc 4 5 in
            let expected_synth_guard = pexp_apply
                                         ~loc:base_loc
                                         (pexp_ident
                                            ~loc:base_loc
                                            { txt = Lident "="; loc = base_loc }
                                         )
                                         [ ( Nolabel
                                           , pexp_ident
                                               ~loc:base_loc
                                               { txt = Lident "y"; loc = base_loc }
                                           )
                                         ; ( Nolabel
                                           , pexp_ident
                                               ~loc:synth_loc
                                               { txt = Lident "_syn_0y"; loc = synth_loc }
                                           )
                                         ]
            in
            assert_equal expected_synth_guard synth_guard ~printer:exp_printer
          end

       | other ->
          failwith ("Expected two unlabelled, got " ^ (string_of_int (List.length other)))
     end
  | other ->
     failwith ("Not expected guard expression: " ^ (exp_printer other))

let suite =
  "Pattern rewrite tests" >:::
    [ "Simple 2-member tuple equality synthesis" >:: test_basic_tuple_eq_rewrite
    ; "Preserve guard in tuple rewrite" >:: test_tuple_rewrite_with_user_guard
    ]

let _ = run_test_tt_main suite

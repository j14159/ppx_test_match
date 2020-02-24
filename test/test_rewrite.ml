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

(* Printer for error reporting.  *)
let pat_printer p =
  let f = Format.str_formatter in
  Pprintast.pattern f p;
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

let simple_tuple_rewrite _ =
  let text_pat = "(x, x)" in
  let pat = Parse.pattern (Lexing.from_string text_pat) in
  let (rewritten, _) = Ppx_test_match.rewrite_patt pat None in
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
  assert_equal expected rewritten ~printer:pat_printer

let suite =
  "Pattern rewrite tests" >:::
    [ "Simple 2-member tuple rewrite" >:: simple_tuple_rewrite

    ]

let _ = run_test_tt_main suite

open Ppxlib

(** Create synthetic variable names for duplicate occurrences.

    For example the following pattern:
      (x, x)
    Will be rewritten as:
      (x, _syn_x0)
 *)
let synth_name base seq_n = "_syn_" ^ (string_of_int seq_n) ^ base

(** Rewrite an optional user-supplied pattern guard to include the equality
    checks required by duplicated variables in the pattern.

    For example the following pattern:

      (x, x)

    Will be rewritten to this pattern and guard:

      (x, _syn_x0) when x = _syn_x0

    If the user supplied a guard like this:

      (x, x) when x > 1.2

    Then the rewrite would produce the following:

      (x, _syn_x0) when x > 1.2 && x = _syn_x0
 *)
let rewrite_guard g rename_table =
  (* Create equality checks for all the synthetic variables created by rewriting
     duplicate variable names in rewrite_patt.
   *)
  let coupler vars =
    let rec memoized vs memo =
      match vs with
      | ({ pexp_loc = loc; _ } as a) :: ((b :: _) as t) ->
         let open Ast_builder.Default in
         let eq = pexp_ident ~loc { txt = Lident "="; loc } in
         let g = pexp_apply ~loc eq [(Nolabel, a); (Nolabel, b)] in
         memoized t (g :: memo)
      | _ :: t ->
         memoized t memo
      | [] ->
         memo
    in
    memoized vars []
  in

  (* Create guards per each original variable name before flattening and linking
     them all with `&&`.
   *)
  let grouped = Hashtbl.to_seq rename_table in
  (* The reverse here is a bit wasteful but it means the variables in the guards
     will be ordered according to their creation, and the first variable
     occurring in the guards will be the "original" variable name.  Seems like
     that may end up being less confusing/misleading in the future.
   *)
  let gs_by_var = Seq.map (fun (_, (_, vs)) -> List.rev vs) grouped
                  |> Seq.map coupler
                  (* Flatten before combining:  *)
                  |> Seq.fold_left (@) []
  in
  (* Chain all guards together with `&&`.  *)
  let full_gs = (Option.to_list g) @ gs_by_var in
  let guard_folder acc next =
    (* We use the accumulator's location for the `&&` application so that a
       user-supplied guard's location is preserved as the location for all
       of them.
     *)
    let { pexp_loc = loc; _ } = acc in
    [%expr [%e acc] && [%e next]]
  in

  match full_gs with
  | h :: ts -> Some (List.fold_left guard_folder h ts)
  | [] -> None

let rewrite_patt p g =
  let tbl = Hashtbl.create 10 in
  (* Rewrites an individual element of a pattern.  *)
  let rec rewrite_item = function
    | { ppat_desc = Ppat_tuple l ; _ } as patt ->
       { patt with ppat_desc = Ppat_tuple (List.map rewrite_item l) }
    | { ppat_desc = Ppat_record (members, closed_flag); _ } as patt ->
       let members2 = List.map (fun (m, p) -> (m, rewrite_item p)) members in
       { patt with ppat_desc = Ppat_record (members2, closed_flag) }
    | { ppat_desc = Ppat_variant (lbl, pat_opt); _ } as patt ->
       let pat_opt2 = Option.map (fun p -> rewrite_item p) pat_opt in
       { patt with ppat_desc = Ppat_variant (lbl, pat_opt2) }
    | { ppat_desc = Ppat_construct (lident, pat_opt); _ } as patt ->
       let pat_opt2 = Option.map (fun p -> rewrite_item p) pat_opt in
       { patt with ppat_desc = Ppat_construct (lident, pat_opt2) }
    | { ppat_desc = Ppat_var ({ txt = label; loc }); _ } as pd ->
       begin
         match Hashtbl.find_opt tbl label with
         | None ->
            (* Tuple is the next synthetic variable number and the list of
               synthesized identifiers.
             *)
            let open Ast_builder.Default in
            let synth_ident = pexp_ident ~loc { txt = Lident label; loc } in
            Hashtbl.add tbl label (0, [synth_ident]);
            pd
         | Some (seq, rewritten_items) ->
            let new_seq = seq + 1 in
            let var_name = synth_name label seq in
            let open Ast_builder.Default in
            let synth_var = ppat_var ~loc { txt = var_name; loc } in
            let synth_ident = pexp_ident ~loc { txt = Lident var_name; loc } in
            Hashtbl.remove tbl label;
            Hashtbl.add tbl label (new_seq, synth_ident :: rewritten_items);
            synth_var
       end
    | other ->
       other
  in

  let p2 = rewrite_item p in
  let g2 = rewrite_guard g tbl in
  p2, g2

(* Format a match-failure message using the pattern and optional guard.  *)
let fail_msg pat guard =
  let f = Format.str_formatter in
  Format.pp_open_box f 0;
  Format.fprintf f "Failed to match";
  Format.pp_print_space f ();
  Pprintast.pattern f pat;
  let _ = Option.map
            (fun g ->
              Format.pp_print_space f ();
              Format.fprintf f "when";
              Format.pp_print_space f ();
              Pprintast.expression f g
            )
            guard
  in
  Format.pp_close_box f ();
  Format.flush_str_formatter ()

let test_match_ext =
  Extension.declare
    "ppx_assert_match.test_match"
    Extension.Context.Expression
    Ast_pattern.(ppat __ __)
    (fun ~loc ~path:_ patt guard ->
      let open Ast_builder.Default in
      let rewritten_patt, rewritten_guard = rewrite_patt patt guard in
      let msg = pexp_constant ~loc (Pconst_string (fail_msg patt guard, None)) in
      (* To simplify pattern construction below we need to always have _some_
         guard expression because I'm using Metaquot.
       *)
      let g = Option.value rewritten_guard ~default:[%expr true] in
      [%expr fun ?printer v ->
          match v with
          | [%p rewritten_patt] when [%e g] -> ()
          | _failed ->
             let full_msg = Option.fold
                              ~none:[%e msg]
                              ~some:(fun p ->
                                let f = Format.str_formatter in
                                Format.pp_open_box f 0;
                                Format.fprintf f "@[%s@ with value@ %s@]\n" [%e msg] (p _failed);
                                Format.flush_str_formatter ()
                              )
                              printer
             in
             failwith full_msg
      ])

(* TODO:  this is pretty brutal duplication.  Need to collapse this.

   I introduced this for some better output when using test_match with OUnit but
   this could _probably_ be much better generalized.  I dabbled briefly but had
   some trouble closing over a general exception-raising function.  Will revisit
   soon.
 *)
let ounit_test_match_ext =
  Extension.declare
    "ppx_assert_match.ounit_match"
    Extension.Context.Expression
    Ast_pattern.(ppat __ __)
    (fun ~loc ~path:_ patt guard ->
      let open Ast_builder.Default in
      let rewritten_patt, rewritten_guard = rewrite_patt patt guard in
      let msg = pexp_constant ~loc (Pconst_string (fail_msg patt guard, None)) in
      (* To simplify pattern construction below we need to always have _some_
         guard expression because I'm using Metaquot.
       *)
      let g = Option.value rewritten_guard ~default:[%expr true] in
      [%expr fun ?printer v ->
          match v with
          | [%p rewritten_patt] when [%e g] -> ()
          | _failed ->
             let full_msg = Option.fold
                              ~none:[%e msg]
                              ~some:(fun p ->
                                let f = Format.str_formatter in
                                Format.pp_open_box f 0;
                                Format.fprintf f "@[%s@ with value:@ %s@]\n" [%e msg] (p _failed);
                                Format.flush_str_formatter ()
                              )
                              printer
             in
             let open OUnitTest in
             raise (OUnit_failure full_msg)
      ])

let _ =
  Driver.register_transformation
    ~extensions:[test_match_ext; ounit_test_match_ext]
    "ppx_test_match"

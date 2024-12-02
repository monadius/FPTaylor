(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* C output for the ErrorBounds tool                                          *)
(* -------------------------------------------------------------------------- *)

open Expr
open Task
open Format

let print_list sep fmt =
  Lib.print_list (pp_print_string fmt) (fun () -> pp_print_string fmt sep)

type var_info = {
  var_name : string;
  (* If prec < 0 then infinite precision is assumed *)
  (* The precision may be modified when rounding expressions are translated to MPFR *)
  mutable var_prec : int;
}

type global_env = {
  mutable constants : (Num.num * var_info) list;
  mutable global_vars : var_info list;
  (* Prefix for global variables and constants *)
  name_prefix : string;
  parameters : (string * var_info) list;
}

type env = {
  global_env : global_env;
  (* Index for temporary local variables *)
  mutable local_index : int;
  (* A list of all local subexpressions and corresponding variables (including constants) *)
  mutable subexprs : (expr * var_info) list;
  (* A list of known variables for subexpressions (e.g., for the return value)*)
  subexprs_names : (expr * var_info) list;
}

let mk_var_info ?(prec = -1) name = { 
  var_name = name; 
  var_prec = prec
}

let mk_global_env ~prefix parameters = {
  parameters = parameters;
  constants = [];
  name_prefix = prefix;
  global_vars = [];
}

let mk_local_env global_env subexprs_names = {
  global_env = global_env;
  local_index = 0;
  subexprs = [];
  subexprs_names = subexprs_names;
}

let get_expr_name env ~local expr =
  try Lib.assoc_eq eq_expr expr env.subexprs, true
  with Not_found ->
    let var_info, flag =
      match expr with
      | Const c when Const.is_rat c -> begin
          let n = Const.to_num c in
          try Lib.assoc_eq Num.eq_num n env.global_env.constants, true
          with Not_found ->
            (* Rational constants are always global *)
            let index = List.length env.global_env.constants in
            let name = sprintf "%sc_%d" env.global_env.name_prefix index in
            let var = mk_var_info name in
            env.global_env.constants <- (n, var) :: env.global_env.constants;
            var, true
        end
      | Var v -> List.assoc v env.global_env.parameters, true
      | _ -> begin
          try Lib.assoc_eq eq_expr expr env.subexprs_names, false
          with Not_found ->
            let var = 
              if local then begin
                env.local_index <- env.local_index + 1;
                let index = env.local_index in
                let name = sprintf "loc_%d" index in
                mk_var_info name
              end else begin
                let index = List.length env.global_env.global_vars in
                let name = sprintf "%st_%d" env.global_env.name_prefix index in
                let var = mk_var_info name in
                env.global_env.global_vars <- var :: env.global_env.global_vars;
                var
              end in
            env.subexprs <- (expr, var) :: env.subexprs;
            var, false 
        end
    in
    env.subexprs <- (expr, var_info) :: env.subexprs;
    var_info, flag

let translate_mpfr env =
  let mpfr_rnd_of_rnd_type = function
    | Some { Rounding.rnd_type = Rounding.Rnd_ne } -> "MPFR_RNDN"
    | Some { Rounding.rnd_type = Rounding.Rnd_up } -> "MPFR_RNDU"
    | Some { Rounding.rnd_type = Rounding.Rnd_down } -> "MPFR_RNDD"
    | Some { Rounding.rnd_type = Rounding.Rnd_0 } -> "MPFR_RNDZ"
    | None -> "MPFR_RNDN"
  in
  let rec translate fmt expr =
    let var_info, found_flag = get_expr_name env ~local:false expr in
    let name = var_info.var_name in
    if found_flag then name
    else
      let () =
        match expr with
        | Rounding (rnd, arg) ->
          let () = match arg with
            | U_op (op, arg) -> translate_unary_op fmt op ~rnd name arg
            | Bin_op (op, arg1, arg2) -> translate_bin_op fmt op ~rnd name arg1 arg2
            | Gen_op (op, args) -> translate_gen_op fmt op ~rnd name args
            | _ ->
              let arg_name = translate fmt arg in
              fprintf fmt "  mpfr_set(%s, %s, %s);@." name arg_name (mpfr_rnd_of_rnd_type (Some rnd))
          in
          (* Modify the precision of the variable which holds the result *)
          var_info.var_prec <- Rounding.type_precision rnd.fp_type
        | U_op (op, arg) -> translate_unary_op fmt op name arg
        | Bin_op (op, arg1, arg2) -> translate_bin_op fmt op name arg1 arg2
        | Gen_op (op, args) -> translate_gen_op fmt op name args
        | _ -> failwith ("translate_mpfr: unsupported operation") in
      name
  and translate_unary_op fmt op ?(rnd : Rounding.rnd_info option) res_name arg =
    let rnd = mpfr_rnd_of_rnd_type rnd in
    let arg_name = translate fmt arg in
    match op with
    | Op_neg -> fprintf fmt "  mpfr_neg(%s, %s, %s);@." res_name arg_name rnd
    | Op_abs -> fprintf fmt "  mpfr_abs(%s, %s, %s);@." res_name arg_name rnd
    | Op_inv -> fprintf fmt "  mpfr_d_div(%s, 1.0, %s, %s);@." res_name arg_name rnd
    | Op_sqrt -> fprintf fmt "  mpfr_sqrt(%s, %s, %s);@." res_name arg_name rnd
    | Op_exp -> fprintf fmt "  mpfr_exp(%s, %s, %s);@." res_name arg_name rnd
    | Op_log -> fprintf fmt "  mpfr_log(%s, %s, %s);@." res_name arg_name rnd
    | Op_sin -> fprintf fmt "  mpfr_sin(%s, %s, %s);@." res_name arg_name rnd
    | Op_cos -> fprintf fmt "  mpfr_cos(%s, %s, %s);@." res_name arg_name rnd
    | _ -> failwith ("translate_mpfr: unsupported unary operation: " ^ u_op_name op)
  and translate_bin_op fmt op ?(rnd : Rounding.rnd_info option) res_name arg1 arg2 =
    let rnd = mpfr_rnd_of_rnd_type rnd in
    let a1 = translate fmt arg1 in
    let a2 = translate fmt arg2 in
    match op with
    | Op_min -> fprintf fmt "  mpfr_min(%s, %s, %s, %s);@." res_name a1 a2 rnd
    | Op_max -> fprintf fmt "  mpfr_max(%s, %s, %s, %s);@." res_name a1 a2 rnd
    | Op_add -> fprintf fmt "  mpfr_add(%s, %s, %s, %s);@." res_name a1 a2 rnd
    | Op_sub -> fprintf fmt "  mpfr_sub(%s, %s, %s, %s);@." res_name a1 a2 rnd
    | Op_mul -> fprintf fmt "  mpfr_mul(%s, %s, %s, %s);@." res_name a1 a2 rnd
    | Op_div -> fprintf fmt "  mpfr_div(%s, %s, %s, %s);@." res_name a1 a2 rnd
    | Op_nat_pow -> begin
        match arg2 with
        | Const (Const.Rat n) when Num.is_integer_num n ->
          fprintf fmt "mpfr_pow_ui(%s, %s, %s, %s);@." res_name a1 (Num.string_of_num n) rnd
        | _ -> failwith "translate_mpfr: Op_nat_pow: non-integer exponent"
      end
    | _ -> failwith ("translate_mpfr: unsupported binary operation: " ^ bin_op_name op)
  and translate_gen_op fmt op ?(rnd : Rounding.rnd_info option) res_name args =
    let rnd = mpfr_rnd_of_rnd_type rnd in
    let arg_names = List.map (translate fmt) args in
    match op, arg_names with
    | Op_fma, [a1; a2; a3] -> fprintf fmt "  mpfr_fma(%s, %s, %s, %s, %s);@." res_name a1 a2 a3 rnd
    | _ -> failwith ("translate_mpfr: unsupported generic operation: " ^ gen_op_name op)
  in
  translate

let translate_mpfi env =
  let rec translate fmt expr =
    let { var_name = name }, found_flag = get_expr_name env ~local:false expr in
    if found_flag then name
    else
      let () =
        match expr with
        | Const c ->
          let x = Const.to_interval c in
          fprintf fmt "  mpfi_interv_d(%s, %.20e, %.20e);@." 
            name x.Interval.low x.Interval.high
        | U_op (op, arg) -> begin
            let arg_name = translate fmt arg in
            match op with
            | Op_neg -> fprintf fmt "  mpfi_neg(%s, %s);@." name arg_name
            | Op_abs -> fprintf fmt "  mpfi_abs(%s, %s);@." name arg_name
            | Op_inv -> fprintf fmt "  mpfi_inv(%s, %s);@." name arg_name
            | Op_sqrt -> fprintf fmt "  mpfi_sqrt(%s, %s);@." name arg_name
            | Op_exp -> fprintf fmt "  mpfi_exp(%s, %s);@." name arg_name
            | Op_log -> fprintf fmt "  mpfi_log(%s, %s);@." name arg_name
            | Op_sin -> fprintf fmt "  mpfi_sin(%s, %s);@." name arg_name
            | Op_cos -> fprintf fmt "  mpfi_cos(%s, %s);@." name arg_name
            | Op_floor_power2 -> fprintf fmt "  mpfi_floor_power2(%s, %s);@." name arg_name
            | _ -> failwith ("translate_mpfi: unsupported unary operation: " ^ u_op_name op)
          end
        | Bin_op (op, arg1, arg2) -> begin
            let a1 = translate fmt arg1 in
            let a2 = translate fmt arg2 in
            match op with
            | Op_min -> fprintf fmt "  mpfi_min(%s, %s, %s);@." name a1 a2
            | Op_max -> fprintf fmt "  mpfi_max(%s, %s, %s);@." name a1 a2
            | Op_add -> fprintf fmt "  mpfi_add(%s, %s, %s);@." name a1 a2
            | Op_sub -> fprintf fmt "  mpfi_sub(%s, %s, %s);@." name a1 a2
            | Op_mul -> fprintf fmt "  mpfi_mul(%s, %s, %s);@." name a1 a2
            | Op_div -> fprintf fmt "  mpfi_div(%s, %s, %s);@." name a1 a2
            | Op_sub2 -> fprintf fmt "  mpfi_sub2(%s, %s, %s);@." name a1 a2
            | Op_nat_pow -> begin
                match arg2 with
                | Const (Const.Rat n) when Num.is_integer_num n ->
                  fprintf fmt "  mpfi_pow_ui(%s, %s, %s);@." name a1 (Num.string_of_num n)
                | _ -> failwith "translate_mpfi: Op_nat_pow: non-integer exponent"
              end
            | _ -> failwith ("translate_mpfi: unsupported binary operation: " ^ bin_op_name op)
          end
        | Gen_op (Op_ulp, [Const p; Const e; arg]) ->
          fprintf fmt "  mpfi_goldberg_ulp(%s, %d, %d, %s);@."
            name (Const.to_int p) (Const.to_int e) (translate fmt arg)
        | _ -> failwith ("translate_mpfi: unsupported operation") in
      name 
  in
  translate

let translate_double env =
  let rec translate fmt expr =
    let var_info, found_flag = get_expr_name env ~local:true expr in
    let name = var_info.var_name ^ "d"in
    if found_flag then name
    else
      let () =
        match expr with
        | U_op (op, arg) -> begin
            let arg_name = translate fmt arg in
            match op with
            | Op_neg -> fprintf fmt "  double %s = -%s;@." name arg_name
            | Op_abs -> fprintf fmt "  double %s = fabs(%s);@." name arg_name
            | Op_inv -> fprintf fmt "  double %s = 1.0 / %s;@." name arg_name
            | Op_sqrt -> fprintf fmt "  double %s = sqrt(%s);@." name arg_name
            | Op_exp -> fprintf fmt "  double %s = exp(%s);@." name arg_name
            | Op_log -> fprintf fmt "  double %s = log(%s);@." name arg_name
            | Op_sin -> fprintf fmt "  double %s = sin(%s);@." name arg_name
            | Op_cos -> fprintf fmt "  double %s = cos(%s);@." name arg_name
            | _ -> failwith ("translate_double: unsupported unary operation: " ^ u_op_name op)
          end
        | Bin_op (op, arg1, arg2) -> begin
            let a1 = translate fmt arg1 in
            let a2 = translate fmt arg2 in
            match op with
            | Op_min -> fprintf fmt "  double %s = fmin(%s, %s);@." name a1 a2
            | Op_max -> fprintf fmt "  double %s = fmax(%s, %s);@." name a1 a2
            | Op_add -> fprintf fmt "  double %s = %s + %s;@." name a1 a2
            | Op_sub -> fprintf fmt "  double %s = %s - %s;@." name a1 a2
            | Op_mul -> fprintf fmt "  double %s = %s * %s;@." name a1 a2
            | Op_div -> fprintf fmt "  double %s = %s / %s;@." name a1 a2
            | _ -> failwith ("translate_double: unsupported binary operation: " ^ bin_op_name op)
          end
        | _ -> failwith ("translate_double: unsupported operation") in
      name 
  in
  translate

let translate_single env =
  let rec translate fmt expr =
    let var_info, found_flag = get_expr_name env ~local:true expr in
    let name = var_info.var_name ^ "f" in
    if found_flag then name
    else
      let () =
        match expr with
        | U_op (op, arg) -> begin
            let arg_name = translate fmt arg in
            match op with
            | Op_neg -> fprintf fmt "  float %s = -%s;@." name arg_name
            | Op_abs -> fprintf fmt "  float %s = fabsf(%s);@." name arg_name
            | Op_inv -> fprintf fmt "  float %s = 1.0f / %s;@." name arg_name
            | Op_sqrt -> fprintf fmt "  float %s = sqrtf(%s);@." name arg_name
            | Op_exp -> fprintf fmt "  float %s = expf(%s);@." name arg_name
            | Op_log -> fprintf fmt "  float %s = logf(%s);@." name arg_name
            | Op_sin -> fprintf fmt "  float %s = sinf(%s);@." name arg_name
            | Op_cos -> fprintf fmt "  float %s = cosf(%s);@." name arg_name
            | _ -> failwith ("translate_single: unsupported unary operation: " ^ u_op_name op)
          end
        | Bin_op (op, arg1, arg2) -> begin
            let a1 = translate fmt arg1 in
            let a2 = translate fmt arg2 in
            match op with
            | Op_min -> fprintf fmt "  float %s = fminf(%s, %s);@." name a1 a2
            | Op_max -> fprintf fmt "  float %s = fmaxf(%s, %s);@." name a1 a2
            | Op_add -> fprintf fmt "  float %s = %s + %s;@." name a1 a2
            | Op_sub -> fprintf fmt "  float %s = %s - %s;@." name a1 a2
            | Op_mul -> fprintf fmt "  float %s = %s * %s;@." name a1 a2
            | Op_div -> fprintf fmt "  float %s = %s / %s;@." name a1 a2
            | _ -> failwith ("translate_single: unsupported binary operation: " ^ bin_op_name op)
          end
        | _ -> failwith ("translate_single: unsupported operation") in
      name 
  in
  translate

let remove_rnd expr =
  let rec remove expr =
    match expr with
    | Const c -> expr
    | Var v -> expr
    | U_op (op, arg) -> U_op (op, remove arg)
    | Bin_op (op, arg1, arg2) -> Bin_op (op, remove arg1, remove arg2)
    | Gen_op (op, args) -> Gen_op (op, List.map remove args)
    | Rounding (rnd, arg) -> remove arg in
  remove expr

let print_init_functions global_env ~name_prefix ?(double = false) ?(single = false) ?(mpfi = false) fmt =
  let mp_type, mp_prefix = if mpfi then "mpfi_t", "mpfi" else "mpfr_t", "mpfr" in
  let c_names = List.map (fun (_, info) -> info.var_name) global_env.constants in
  let c_names_double = List.map (fun (_, info) -> info.var_name ^ "d") global_env.constants in
  let c_names_single = List.map (fun (_, info) -> info.var_name ^ "f") global_env.constants in
  let global_var_names = List.map (fun var -> var.var_name) global_env.global_vars in
  let global_vars_flag = List.length global_var_names > 0 in
  let const_flag = List.length c_names > 0 in
  if global_vars_flag then
    fprintf fmt "static %s %a;@." mp_type (print_list ", ") global_var_names;
  if const_flag then
    fprintf fmt "static %s %a;@." mp_type (print_list ", ") c_names;
  if double && List.length c_names_double > 0 then
    fprintf fmt "static double %a;@." (print_list ", ") c_names_double;
  if single && List.length c_names_single > 0 then
    fprintf fmt "static float %a;@." (print_list ", ") c_names_single;
  pp_print_newline fmt ();
  fprintf fmt "void %s_init() {@." name_prefix;
  let init_var var =
    if var.var_prec < 0 then
      fprintf fmt "  %s_init(%s);@." mp_prefix var.var_name
    else
      fprintf fmt "  %s_init2(%s, %d);@." mp_prefix var.var_name var.var_prec in
  let init_constant (n, info) =
    let f_const = if single then sprintf "&%sf" info.var_name else "NULL" in
    let d_const = if double then sprintf "&%sd" info.var_name else "NULL" in
      fprintf fmt "  init_constants(\"%s\", MPFR_RNDN, %s, %s, %s);@."
        (Num.string_of_num n) f_const d_const info.var_name in
  List.iter init_var global_env.global_vars;
  List.iter init_var (List.map snd global_env.constants);
  List.iter init_constant global_env.constants;
  fprintf fmt "}@.";
  pp_print_newline fmt ();
  fprintf fmt "void %s_clear() {@." name_prefix;
  if global_vars_flag then
    List.iter (fun name -> fprintf fmt "  %s_clear(%s);@." mp_prefix name) global_var_names;
  if const_flag then
    List.iter (fun name -> fprintf fmt "  %s_clear(%s);@." mp_prefix name) c_names;
  fprintf fmt "}@."

let print_mp_f global_env base_name ?(result_in_rop = true) ?(mpfi = false) ?(index = 1) fmt expr =
  (* result_in_rop = false when we want to round the result to a custom precision *)
  let env = mk_local_env global_env (if result_in_rop then [expr, mk_var_info "r_op"] else []) in
  let mp_prefix = if mpfi then "mpfi" else "mpfr" in
  let args = List.map (fun (_, { var_name = name }) -> mp_prefix ^ "_srcptr " ^ name) env.global_env.parameters in
  let body, result_name =
    Lib.write_to_string_result 
      (if mpfi then (translate_mpfi env) else (translate_mpfr env))
      expr in
  let f_name = base_name ^ (if index <= 1 then "" else string_of_int index) in
  fprintf fmt "void %s(%s r_op, %a) {@." f_name (mp_prefix ^ "_ptr") (print_list ", ") args;
  fprintf fmt "%s" body;
  if result_name <> "r_op" then begin
    if mpfi then 
      fprintf fmt "  mpfi_set(r_op, %s);@." result_name
    else
      fprintf fmt "  mpfr_set(r_op, %s, MPFR_RNDN);@." result_name
  end;
  fprintf fmt "}@."

let print_double_f global_env fmt expr =
  let env = mk_local_env global_env [] in
  let args = List.map (fun (_, { var_name = name }) -> "double " ^ name ^ "d") env.global_env.parameters in
  let body, result_name =
    Lib.write_to_string_result (translate_double env) expr in
  fprintf fmt "double f_64(%a) {@." (print_list ", ") args;
  fprintf fmt "%s  return %s;@.}@." body result_name

let print_single_f global_env fmt expr =
  let env = mk_local_env global_env []in
  let args = List.map (fun (_, { var_name = name }) -> "float " ^ name ^ "f") env.global_env.parameters in
  let body, result_name =
    Lib.write_to_string_result (translate_single env) expr in
  fprintf fmt "float f_32(%a) {@." (print_list ", ") args;
  fprintf fmt "%s  return %s;@.}@." body result_name

let print_init_f_and_mps global_env fmt ?double ?single ?mpfi exprs =
  let mps = List.mapi 
              (fun i e -> 
                Lib.write_to_string (print_mp_f global_env ?mpfi ~index:(i + 1) "f_high") e) exprs in
  print_init_functions global_env ~name_prefix:"f_high" ?double ?single ?mpfi fmt;
  pp_print_newline fmt ();
  List.iter (fprintf fmt "%s@.") mps

let print_mpfr_low global_env fmt expr =
  let f_low = Lib.write_to_string
    (print_mp_f global_env "f_low" ~result_in_rop:false ~mpfi:false ~index:1) expr in
  print_init_functions global_env ~name_prefix:"f_low" ~double:false ~single:false ~mpfi:false fmt;
  fprintf fmt "@.%s@." f_low

let print_init_and_clear fmt name_prefixes =
  fprintf fmt "void f_init() {@.";
  List.iter (fun name -> fprintf fmt "  %s_init();@." name) name_prefixes;
  fprintf fmt "}@.@.";
  fprintf fmt "void f_clear() {@.";
  List.iter (fun name -> fprintf fmt "  %s_clear();@." name) name_prefixes;
  fprintf fmt "}@."

let generate_error_bounds fmt task =
  let task_vars, var_bounds = 
    let vars = all_active_variables task in
    let bounds = List.map (variable_interval task) vars in
    match vars with
    | [] -> ["unused"],
            let names = all_variables task in
            if names <> [] then
              [variable_interval task (List.hd names)]
            else
              [{Interval.low = 1.; Interval.high = 2.}]
    | _ -> vars, bounds in
  let var_names = List.map (fun s -> "v_" ^ ExprOut.fix_name s) task_vars in
  let var_infos = List.map mk_var_info var_names in
  let parameters = List.combine task_vars var_infos in
  let global_env = mk_global_env ~prefix:"" parameters in
  (* We need to create MPFI functions in a separate global environment to avoid extra global variables *)
  let global_env_mpfi = mk_global_env ~prefix:"" parameters in
  let no_rnd_expr = remove_rnd task.expression in
  fprintf fmt "#ifdef USE_MPFI@.";
  fprintf fmt "@.#include \"search_mpfi.h\"@.";
  pp_print_newline fmt ();
  print_init_f_and_mps global_env_mpfi fmt ~double:true ~single:true ~mpfi:true [no_rnd_expr];
  fprintf fmt "@.#else@.";
  fprintf fmt "@.#include \"search_mpfr.h\"@.";
  pp_print_newline fmt ();
  print_init_f_and_mps global_env fmt ~double:true ~single:true ~mpfi:false [no_rnd_expr];
  fprintf fmt "@.#endif@.";
  pp_print_newline fmt ();
  let low_str = List.map (fun b -> sprintf "%.20e" b.Interval.low) var_bounds in
  let high_str = List.map (fun b -> sprintf "%.20e" b.Interval.high) var_bounds in
  fprintf fmt "const double low[] = {%a};@." (print_list ", ") low_str;
  fprintf fmt "const double high[] = {%a};@." (print_list ", ") high_str;
  fprintf fmt "const char *f_name = \"%s\";@." task.name;
  pp_print_newline fmt ();
  print_double_f global_env fmt no_rnd_expr;
  pp_print_newline fmt ();
  print_single_f global_env fmt no_rnd_expr;
  pp_print_newline fmt ();
  (* Generate an MPFR representation of the task expression *)
  (* Note: subnormal numbers are not correctly handled by this representation *)
  print_mpfr_low (mk_global_env ~prefix:"m" parameters) fmt task.expression;
  print_init_and_clear fmt ["f_high"; "f_low"]

let generate_data_functions fmt task named_exprs =
  let task_vars, var_bounds =
    let vars = all_active_variables task in
    let bounds = List.map (variable_interval task) vars in
    match vars with
    | [] -> ["unused"],
            let names = all_variables task in
            if names <> [] then
              [variable_interval task (List.hd names)]
            else
              [{Interval.low = 1.; Interval.high = 2.}]
    | _ -> vars, bounds in
  let var_names = List.map (fun s -> "v_" ^ ExprOut.fix_name s) task_vars in
  let var_infos = List.map mk_var_info var_names in
  let global_env = mk_global_env ~prefix:"" (List.combine task_vars var_infos) in
  fprintf fmt "#include \"data_mpfi.h\"@.";
  fprintf fmt "#include \"func.h\"@.";
  pp_print_newline fmt ();
  let expr_names, exprs = List.split named_exprs in
  print_init_f_and_mps global_env fmt ~mpfi:true exprs;
  pp_print_newline fmt ();
  let low_str = List.map (fun b -> sprintf "%.20e" b.Interval.low) var_bounds in
  let high_str = List.map (fun b -> sprintf "%.20e" b.Interval.high) var_bounds in
  fprintf fmt "const double low[] = {%a};@." (print_list ", ") low_str;
  fprintf fmt "const double high[] = {%a};@." (print_list ", ") high_str;
  let expr_names = List.map (sprintf "\"%s\"") expr_names in
  fprintf fmt "const char *f_names[] = {%a};@." (print_list ", ") expr_names;
  let n = List.length exprs in
  let f_names = 
    Lib.init_list n (fun i -> "f_high" ^ (if i = 0 then "" else string_of_int (i + 1))) in
  fprintf fmt "const int n_funcs = %d;@." (List.length f_names);
  fprintf fmt "const F_HIGH funcs[] = {%a};@." (print_list ", ") f_names;
  fprintf fmt "const char *expression_string = \"%s\";@."
    (ExprOut.Info.print_str (remove_rnd task.expression));
  print_init_and_clear fmt ["f_high"]

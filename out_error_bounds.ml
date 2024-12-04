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

type var_type = TypeDouble | TypeSingle | TypeMPFR | TypeMPFI

type var_info = {
  var_name : string;
  var_type : var_type;
  (* var_prec is only used for MPFR variables *)
  (* If prec < 0 then infinite precision is assumed. *)
  (* The precision may be modified when rounding expressions are translated to MPFR. *)
  mutable var_prec : int;
}

type env = {
  (* Prefix for global variables and constants *)
  name_prefix : string;
  parameters : (string * var_info) list;
  (* A list of known variables for subexpressions (e.g., for the return value)*)
  subexprs_names : (expr * var_info) list;
  (* A list of constants. All constants are globally initialized. *)
  mutable constants : (Num.num * var_info) list;
  (* A list of global variables. *)
  mutable global_vars : var_info list;
  (* Index for temporary local variables *)
  mutable local_index : int;
  (* A list of all local subexpressions and corresponding variables (including constants) *)
  mutable subexprs : (expr * var_info) list;
}

let var_type_name ~is_input = function
  | TypeDouble -> if is_input then "const double" else "double"
  | TypeSingle -> if is_input then "const float" else "float"
  | TypeMPFR -> if is_input then "mpfr_srcptr" else "mpfr_t"
  | TypeMPFI -> if is_input then "mpfi_srcptr" else "mpfi_t"

let mk_var_info ?(prec = -1) name var_type = { 
  var_name = name; 
  var_type = var_type;
  var_prec = prec
}

let mk_env name_prefix parameters subexprs_names = {
  name_prefix = name_prefix;
  parameters = parameters;
  constants = [];
  global_vars = [];
  local_index = 0;
  subexprs = [];
  subexprs_names = subexprs_names;
}

let get_expr_name env ~local ~var_type expr =
  try Lib.assoc_eq eq_expr expr env.subexprs, true
  with Not_found ->
    let var_info, flag =
      match expr with
      | Const c when Const.is_rat c -> begin
          let n = Const.to_num c in
          try Lib.assoc_eq Num.eq_num n env.constants, true
          with Not_found ->
            (* Rational constants are always global: ignore local flag *)
            let index = List.length env.constants in
            let name = sprintf "%sc_%d" env.name_prefix index in
            let var = mk_var_info name var_type in
            env.constants <- (n, var) :: env.constants;
            var, true
        end
      | Var v -> List.assoc v env.parameters, true
      | _ -> begin
          try Lib.assoc_eq eq_expr expr env.subexprs_names, false
          with Not_found ->
            let var = 
              if local then begin
                env.local_index <- env.local_index + 1;
                let index = env.local_index in
                let name = sprintf "loc_%d" index in
                mk_var_info name var_type
              end else begin
                let index = List.length env.global_vars in
                let name = sprintf "%st_%d" env.name_prefix index in
                let var = mk_var_info name var_type in
                env.global_vars <- var :: env.global_vars;
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
    let var, found_flag = get_expr_name env ~local:false ~var_type:TypeMPFR expr in
    let name = var.var_name in
    if found_flag then 
      name
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
          var.var_prec <- Rounding.type_precision rnd.fp_type
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
    let { var_name = name }, found_flag = get_expr_name env ~local:false ~var_type:TypeMPFI expr in
    if found_flag then 
      name
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
    let { var_name = name }, found_flag = get_expr_name env ~local:true ~var_type:TypeDouble expr in
    if found_flag then 
      name
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
    let { var_name = name }, found_flag = get_expr_name env ~local:true ~var_type:TypeSingle expr in
    if found_flag then 
      name
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

let print_init_functions env ~name fmt =
  let all_global_vars = env.global_vars @ List.map snd env.constants in
  (* Declare variables *)
  all_global_vars
  |> Lib.group_by (fun var -> var.var_type)
  |> List.iter (fun (ty, vars) ->
    let type_name = var_type_name ~is_input:false ty in
    fprintf fmt "static %s %a;@." type_name (print_list ", ") (List.map (fun var -> var.var_name) vars)
  );
  pp_print_newline fmt ();
  (* Initialize variables and constants *)
  fprintf fmt "void %s_init() {@." name;
  all_global_vars
  |> List.iter (fun var ->
    match var.var_type with
    | TypeMPFR when var.var_prec < 0 -> fprintf fmt "  mpfr_init(%s);@." var.var_name
    | TypeMPFR -> fprintf fmt "  mpfr_init2(%s, %d);@." var.var_name var.var_prec
    | TypeMPFI -> fprintf fmt "  mpfi_init(%s);@." var.var_name
    | TypeDouble | TypeSingle -> ()
  );
  env.constants
  |> List.iter (fun (n, info) ->
    fprintf fmt "  init_constants(\"%s\", MPFR_RNDN, %s);@."
      (Num.string_of_num n)
      (match info.var_type with
       | TypeSingle -> sprintf "&%s, NULL, NULL, NULL" info.var_name
       | TypeDouble -> sprintf "NULL, &%s, NULL, NULL" info.var_name
       | TypeMPFR -> sprintf "NULL, NULL, %s, NULL" info.var_name
       | TypeMPFI -> sprintf "NULL, NULL, NULL, %s" info.var_name)
  );
  fprintf fmt "}@.@.";
  (* Clear variables and constants *)
  fprintf fmt "void %s_clear() {@." name;
  all_global_vars
  |> List.iter (fun var ->
    match var.var_type with
    | TypeMPFR -> fprintf fmt "  mpfr_clear(%s);@." var.var_name
    | TypeMPFI -> fprintf fmt "  mpfi_clear(%s);@." var.var_name
    | TypeDouble | TypeSingle -> ()
  );
  fprintf fmt "}@."

let print_multiprecision name env ?(mpfi = false) fmt expr =
  let mp_prefix = if mpfi then "mpfi" else "mpfr" in
  let args = 
    List.map (fun (_, var) -> sprintf "%s %s" (var_type_name ~is_input:true var.var_type) var.var_name) 
    env.parameters in
  fprintf fmt "void %s(%s rop, %a) {@." name (mp_prefix ^ "_ptr") (print_list ", ") args;
  let result_name = (if mpfi then translate_mpfi else translate_mpfr) env fmt expr in
  if result_name <> "rop" then begin
    if mpfi then 
      fprintf fmt "  mpfi_set(rop, %s);@." result_name
    else
      fprintf fmt "  mpfr_set(rop, %s, MPFR_RNDN);@." result_name
  end;
  fprintf fmt "}@."

let print_double_f name env fmt expr =
  let args = 
    List.map (fun (_, var) -> sprintf "%s %s" (var_type_name ~is_input:true var.var_type) var.var_name) 
    env.parameters in
  fprintf fmt "double %s(%a) {@." name (print_list ", ") args;
  let result_name = translate_double env fmt expr in
  fprintf fmt "  return %s;@.}@." result_name

let print_single_f name env fmt expr =
  let args = 
    List.map (fun (_, var) -> sprintf "%s %s" (var_type_name ~is_input:true var.var_type) var.var_name) 
    env.parameters in
  fprintf fmt "float %s(%a) {@." name (print_list ", ") args;
  let result_name = translate_single env fmt expr in
  fprintf fmt "  return %s;@.}@." result_name

let print_expr_functions ~name f_printer env fmt expr =
  let f_str = Lib.write_to_string (f_printer name env) expr in
  print_init_functions env ~name fmt;
  fprintf fmt "@.%s" f_str

let print_init_and_clear fmt names =
  fprintf fmt "void f_init() {@.";
  List.iter (fun name -> fprintf fmt "  %s_init();@." name) names;
  fprintf fmt "}@.@.";
  fprintf fmt "void f_clear() {@.";
  List.iter (fun name -> fprintf fmt "  %s_clear();@." name) names;
  fprintf fmt "}@."

let generate_error_bounds fmt task =
  let no_rnd_expr = remove_rnd task.expression in
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
  let env prefix ?ret_expr ty = 
    mk_env prefix 
      (List.map (fun name -> name, mk_var_info ("v_" ^ ExprOut.fix_name name) ty) task_vars)
      (match ret_expr with
        | Some expr -> [expr, mk_var_info "rop" ty]
        | None -> []) in
  fprintf fmt "@.#include \"mp_common.h\"@.@.";
  fprintf fmt "#ifdef USE_MPFI@.@.";
  (* MPFI f_high *)
  print_expr_functions 
    ~name:"f_high" 
    (print_multiprecision ~mpfi:true) 
    (env "" ~ret_expr:no_rnd_expr TypeMPFI) 
    fmt no_rnd_expr;
  fprintf fmt "@.#else@.@.";
  (* MPFR f_high *)
  print_expr_functions 
    ~name:"f_high" 
    (print_multiprecision ~mpfi:false) 
    (env "" ~ret_expr:no_rnd_expr TypeMPFR) 
    fmt no_rnd_expr;
  fprintf fmt "@.#endif@.@.";
  (* f_64 *)
  print_expr_functions ~name:"f_64" print_double_f (env "d" TypeDouble) fmt no_rnd_expr;
  pp_print_newline fmt ();
  (* f_32 *)
  print_expr_functions ~name:"f_32" print_single_f (env "s" TypeSingle) fmt no_rnd_expr;
  pp_print_newline fmt ();
  (* f_low (computed using MPFR) *)
  (* Note: subnormal numbers are not correctly handled by this representation *)
  print_expr_functions 
    ~name:"f_low" 
    (print_multiprecision ~mpfi:false) 
    (* Do not set ret_expr because we want to round the result to a custom precision *)
    (env "m" TypeMPFR) 
    fmt task.expression;
  pp_print_newline fmt ();
  print_init_and_clear fmt ["f_high"; "f_64"; "f_32"; "f_low"];
  pp_print_newline fmt ();
  (* Print bounds of variables and the task name*)
  let low_str = List.map (fun b -> sprintf "%.20e" b.Interval.low) var_bounds in
  let high_str = List.map (fun b -> sprintf "%.20e" b.Interval.high) var_bounds in
  fprintf fmt "const double low[] = {%a};@." (print_list ", ") low_str;
  fprintf fmt "const double high[] = {%a};@." (print_list ", ") high_str;
  fprintf fmt "const char *f_name = \"%s\";@.@." task.name

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
  let env prefix ret_expr ty = 
    mk_env prefix 
      (List.map (fun name -> name, mk_var_info ("v_" ^ ExprOut.fix_name name) ty) task_vars)
      [ret_expr, mk_var_info "rop" ty] in
  fprintf fmt "#include \"data_mpfi.h\"@.";
  fprintf fmt "#include \"func.h\"@.";
  pp_print_newline fmt ();
  let expr_names, exprs = List.split named_exprs in
  (* Print MPFI functions for all expressions *)
  exprs
  |> List.iteri (fun i expr ->
    print_expr_functions 
      ~name:("f_high" ^ string_of_int (i + 1)) 
      (print_multiprecision ~mpfi:true) 
      (env ("m" ^ string_of_int (i + 1)) expr TypeMPFI) 
      fmt expr;
    pp_print_newline fmt ()
  );
  let low_str = List.map (fun b -> sprintf "%.20e" b.Interval.low) var_bounds in
  let high_str = List.map (fun b -> sprintf "%.20e" b.Interval.high) var_bounds in
  fprintf fmt "const double low[] = {%a};@." (print_list ", ") low_str;
  fprintf fmt "const double high[] = {%a};@." (print_list ", ") high_str;
  let expr_names = List.map (sprintf "\"%s\"") expr_names in
  fprintf fmt "const char *f_names[] = {%a};@." (print_list ", ") expr_names;
  let n = List.length exprs in
  let f_names = List.init n (fun i -> "f_high" ^ string_of_int (i + 1)) in
  fprintf fmt "const int n_funcs = %d;@." (List.length f_names);
  fprintf fmt "const F_MPFI funcs[] = {%a};@." (print_list ", ") f_names;
  fprintf fmt "const char *expression_string = \"%s\";@."
    (ExprOut.Info.print_str (remove_rnd task.expression));
  pp_print_newline fmt ();
  print_init_and_clear fmt f_names

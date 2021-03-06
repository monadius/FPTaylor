(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* FPTaylor JS's main functions                                               *)
(* -------------------------------------------------------------------------- *)

open Js_of_ocaml

module Out = ExprOut.Make(ExprOut.JavaScriptPrinter)

let init_out ty chan =
  let flush s = 
    let obj = object%js
      val ty = ty
      val str = Js.string s
    end in
    Worker.post_message obj in
  Sys_js.set_channel_flusher chan flush

let init_fs () =
  Sys_js.create_file "./default.cfg" "";
  Sys_js.create_file "user.cfg" "verbosity = 1";
  Sys_js.create_file "input.txt" "";
  ()

let validate_options_js () =
  let open Config in
  if get_bool_option "proof-record" then begin
    Log.warning "FPTaylorJS: Proof certificates cannot be created";
    set_option "proof-record" "false"
  end;
  if get_string_option "opt" <> "bb-eval" then begin
    Log.warning "FPTaylorJS: the optimization option '%s' is not supported" (get_string_option "opt");
    set_option "opt" "bb-eval"
  end;
  ()

let run_fptaylor () =
  try
    Log.report `Main "FPTaylor, version %s" Version.version;
    Config.init ["user.cfg"];
    validate_options_js ();
    Fptaylor.validate_options ();
    Fptaylor.fptaylor ["input.txt"]
  with 
  | Failure msg -> Log.error_str msg; []
  | Parsing.Parse_error -> Log.error_str "Parsing error"; []
  | e ->  Log.error_str "Unknown error"; raise e

class type js_msg_type = object
  method input : Js.js_string Js.t Js.readonly_prop
  method defaultcfg : Js.js_string Js.t Js.readonly_prop
  method config : Js.js_string Js.t Js.readonly_prop
end

let js_array_of_interval x =
  Js.array [|x.Interval.low; x.Interval.high|]

let js_opt_array_of_interval = function
| Some x -> Js.some (js_array_of_interval x)
| None -> Js.null

let js_string_of_number_hi prec x =
  Js.string (More_num.string_of_float_hi prec x)

let js_string_of_high prec x =
  js_string_of_number_hi prec x.Interval.high

let js_opt_string_of_high prec = function
| Some x -> Js.some (js_string_of_high prec x)
| None -> Js.null

let js_expr_obj_of_opt_expr task = function
| None -> Js.null
| Some expr ->
  let var_names = Expr.vars_in_expr expr in
  let var_intervals = List.map (Task.variable_interval task) var_names in
  if List.length var_names > 1 then Js.null
  else
    let var_names = List.map (fun v -> "var_" ^ ExprOut.fix_name v) var_names in
    let es = Expr.expr_ref_list_of_expr expr in
    let n = List.length es in
    let body = es 
      |> List.mapi (fun i e ->
          if i < n - 1 then 
            Format.sprintf "var ref_%d = %s;" i (Out.print_str e)
          else
            Format.sprintf "return %s;" (Out.print_str e))
      |> String.concat "\n" in
    let str = Format.sprintf "function(%s) {\n%s\n}"
      (String.concat ", " var_names) body in
    Js.some (object%js
      val expr = Js.string str
      val dom =
        match var_intervals with
        | [x] -> Js.some (js_array_of_interval x)
        | _ -> Js.null
    end)

let process (msg : js_msg_type Js.t) =
  let input = msg##.input |> Js.to_string in
  let default_config = msg##.defaultcfg |> Js.to_string in
  let config = msg##.config |> Js.to_string in
  Sys_js.update_file "input.txt" input;
  Sys_js.update_file "./default.cfg" default_config;
  Sys_js.update_file "user.cfg" config;
  let results = run_fptaylor () in
  let prec = Config.get_int_option "print-precision" in
  let js_of_error_result task r =
    let open Fptaylor in
    object%js
      val errorName = error_type_name r.error_type |> Js.string
      val error = js_opt_array_of_interval r.error
      val errorStr = js_opt_string_of_high prec r.error
      val total2 = js_opt_array_of_interval r.total2
      val total2Str = js_opt_string_of_high prec r.total2
      val errorModel = js_expr_obj_of_opt_expr task r.error_model
    end in
  let res_msg = results
    |> List.map (fun res ->
      let open Fptaylor in
      object%js
        val name = Js.string res.task.name
        val elapsedTime = res.elapsed_time
        val realBounds = js_array_of_interval res.real_bounds
        val realBoundsStr = [|res.real_bounds.low; res.real_bounds.high|] 
          |> Array.map (js_string_of_number_hi prec)
          |> Js.array
        val errors = res.errors
          |> Array.of_list
          |> Array.map (js_of_error_result res.task)
          |> Js.array
      end)
    |> Array.of_list
    |> Js.array in
  Worker.post_message res_msg

let () =
  init_out 1 Stdlib.stdout;
  init_out 2 Stdlib.stderr;
  init_fs ();
  Worker.set_onmessage process

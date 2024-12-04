(* ========================================================================== *)
(*      FPTaylor: A Tool for Rigorous Estimation of Round-off Errors          *)
(*                                                                            *)
(*      Author: Alexey Solovyev, University of Utah                           *)
(*                                                                            *)
(*      This file is distributed under the terms of the MIT license           *)
(* ========================================================================== *)

(* -------------------------------------------------------------------------- *)
(* Auxiliary functions                                                        *)
(* Some code is from HOL Light (lib.ml), the Flyspeck project, and nlcertify  *)
(* -------------------------------------------------------------------------- *)

(* -------------------------------------------------------------------------- *)
(* "Set" operations on lists.                                                 *)
(* -------------------------------------------------------------------------- *)

let rec last = function
  | [] -> failwith "last"
  | [h] -> h
  | _ :: t -> last t

let insert x l =
  if List.mem x l then l else x :: l

let union l1 l2 = List.fold_left (fun r x -> insert x r) l2 l1

let unions l = List.fold_left union [] l

let intersect l1 l2 = List.filter (fun x -> List.mem x l2) l1

let subtract l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1

let rec rev_assoc a = function
  | (v, k) :: t -> if k = a then v else rev_assoc a t
  | [] -> raise Not_found

let rec assoc_eq eq a = function
  | (k, v) :: t -> if eq k a then v else assoc_eq eq a t
  | [] -> raise Not_found

let rec assocd d a = function
  | (k, v) :: t -> if k = a then v else assocd d a t
  | [] -> d

let rec assocd_eq eq d a = function
  | (k, v) :: t -> if eq k a then v else assocd_eq eq d a t
  | [] -> d

let rec (--) = fun m n -> if m > n then [] else m::((m + 1) -- n);;

let enumerate =
  let rec enum acc i = function
    | v :: vs -> enum ((i, v) :: acc) (succ i) vs
    | [] -> List.rev acc
  in
  fun start list ->
  enum [] start list

let group_by f xs =
  let groups = Hashtbl.create 10 in
  let keys = ref [] in
  List.iter (fun x ->
    let key = f x in
    let ys = try Hashtbl.find groups key with Not_found -> keys := key :: !keys; [] in
    Hashtbl.replace groups key (x :: ys)
  ) xs;
  List.map (fun k -> (k, List.rev (Hashtbl.find groups k))) !keys
 
(* -------------------------------------------------------------------------- *)
(* String operations                                                          *)
(* -------------------------------------------------------------------------- *)

let print_list fp sep =
  let rec print = function
    | [] -> ()
    | [s] -> fp s
    | s1 :: s2 :: rest -> fp s1; sep(); print (s2 :: rest) in
  print

let slice ~first ?last str =
  let n = String.length str in
  let last = match last with None -> n | Some v -> v in
  if last <= first || first < 0 || first >= n then ""
  else
    String.sub str first (min last n - first)

let rec concat_env_paths paths =
  match paths with
  | [] -> ""
  | path :: rest -> 
    let path = String.trim path in
    if path = "" then concat_env_paths rest
    else path ^ ":" ^ concat_env_paths rest

(* -------------------------------------------------------------------------- *)
(* IO operations.                                                             *)
(* -------------------------------------------------------------------------- *)

let load_and_close_channel do_close ic = 
  let rec lf ichan a = 
    try
      lf ic (Stdlib.input_line ic :: a)
    with End_of_file -> a in
  let rs = lf ic [] in
  if do_close then Stdlib.close_in ic;
  List.rev rs

let load_file filename = 
  let ic = Stdlib.open_in filename in
  load_and_close_channel true ic

let run_cmd cmd =
  let (ic, oc) = Unix.open_process cmd in
  let s = load_and_close_channel false ic in
  let _ = Unix.close_process (ic, oc) in
  s

let write_to_file fname writer arg =
  let oc = open_out fname in
  let fmt = Format.formatter_of_out_channel oc in
  let result = writer fmt arg in
  Format.pp_print_flush fmt ();
  close_out oc;
  result

let write_to_string writer arg =
  let buf = Buffer.create 100 in
  let fmt = Format.formatter_of_buffer buf in
  ignore (writer fmt arg);
  Format.pp_print_flush fmt ();
  Buffer.contents buf

let write_to_string_result writer arg =
  let buf = Buffer.create 100 in
  let fmt = Format.formatter_of_buffer buf in
  let result = writer fmt arg in
  Format.pp_print_flush fmt ();
  Buffer.contents buf, result

(* Creates all missing directories in a given path *)
let make_path ?(perm = 0o777) path =
  let rec make path =
    if Sys.file_exists path then
      if Sys.is_directory path then ()
      else failwith ("make_path: " ^ path)
    else
      begin
        make (Filename.dirname path);
        Unix.mkdir path perm
      end
  in
  make path

(* Creates a directory if it doesn't exist and returns its name *)
let get_dir dir_name =
  make_path dir_name; 
  dir_name

let set_tmp_dir, get_tmp_dir =
  let tmp_dir = ref "tmp" in
  let set_tmp_dir path = tmp_dir := get_dir path in
  let get_tmp_dir () = get_dir !tmp_dir in
  set_tmp_dir, get_tmp_dir

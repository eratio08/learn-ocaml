open Base
open Stdio

exception Empty_list

let list_max = function
  | [] -> raise Empty_list
  | hd :: tl -> List.fold tl ~init:hd ~f:Int.max
;;


(* Will also turn off backtraces for this file *)
Backtrace.Exn.set_recording  false

let () =
  printf "%d\n" (list_max [ 1; 2; 3 ]);
  printf "%d\n" (list_max [])
;;

(* Will return most recent backtraces *)
Backtrace.Exn.most_recent;;

(* Base has backtraces enabled by default *)
(* Using OCAMLRUNPARAM=b=0 will turn them off *)

(* Disabling backtrace speeds up the execution *)

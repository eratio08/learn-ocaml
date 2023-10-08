open Base
open Stdio

let parse_line line =
  String.split_on_chars ~on:[ ',' ] line |> List.map ~f:Float.of_string
;;

let load filename =
  let inc = In_channel.create filename in
  Exn.protect
    ~f:(fun () -> In_channel.input_lines inc |> List.map ~f:parse_line)
    ~finally:(fun () -> In_channel.close inc)
;;

let load' filename =
  In_channel.with_file filename ~f:(fun inc ->
    In_channel.input_lines inc |> List.map ~f:parse_line)
;;

exception Key_not_found of string

let rec find_exn alist key =
  match alist with
  | [] -> raise (Key_not_found key)
  | (key', data) :: tl -> if String.( = ) key key' then data else find_exn tl key
;;

let lookup_weight ~compute_weight alist key =
  try
    let data = find_exn alist key in
    compute_weight data
  with
  | Key_not_found _ -> 0.
;;

(* to not catch any special type but any exception *)
let lookup_weight' ~compute_weight alist key =
  match
    try Some (find_exn alist key) with
    | _ -> None
  with
  | None -> 0
  | Some data -> compute_weight data
;;

(* pattern match work as well *)
let lookup_weight'' ~compute_weight alist key =
  match find_exn alist key with
  | exception _ -> 0.
  | data -> compute_weight data
;;

(* Use assoc list to avoid any exception *)
let lookup_weight''' ~compute_weight alist key =
  match List.Assoc.find ~equal:String.equal alist key with
  | None -> 0.
  | Some data -> compute_weight data
;;

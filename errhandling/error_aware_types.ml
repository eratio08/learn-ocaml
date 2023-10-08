open Base

exception Key_not_found of string

let rec find_exn alist key =
  match alist with
  | [] -> raise (Key_not_found key)
  | (key', data) :: tl -> if String.( = ) key key' then data else find_exn tl key
;;

let find alist key = Option.try_with (fun () -> find_exn alist key)
let arr = [ "a", 1; "b", 2 ];;

(* None *)
find arr "c";;

(* Some 2 *)
find arr "b"

let find' alist key = Or_error.try_with (fun () -> find_exn alist key);;

(* Base__.Result.Error ("Key_not_found(\"c\")")*)
find' arr "c";;

(* raise exn with *)
(* will return 2 *)
find' arr "b" |> Or_error.ok_exn;;

(* will raise exception *)
find' arr "c" |> Or_error.ok_exn

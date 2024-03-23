open Base

(* declaring new exception *)
exception Key_not_found of string

let raise_exp _ = raise (Key_not_found "test")
let exceptions = [ Division_by_zero; Key_not_found "b" ]

(* the type system show exceptions as exn *)
let example =
  List.filter exceptions ~f:(function
    | Key_not_found _ -> true
    | _ -> false)
;;

let rec find_exn alist key =
  match alist with
  | [] -> raise (Key_not_found key)
  | (key', data) :: tl -> if String.( = ) key key' then data else find_exn tl key
;;

let alist = [ "a", 1; "b", 2 ]
let no_exn = find_exn alist "a"
let will_raise_exn _ = find_exn alist "c"

let merge_lists xs ys ~f =
  if List.length xs <> List.length ys
  then None
  else (
    let rec loop xs ys =
      match xs, ys with
      | [], [] -> []
      | x :: xs, y :: ys -> f x y :: loop xs ys
      (* will throw *)
      | _ -> assert false
    in
    Some (loop xs ys))
;;

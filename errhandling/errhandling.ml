open Base

let error_from_string = Error.of_string "something went wrong"
let float_of_string s = Or_error.try_with (fun () -> Float.of_string s)

(* use s-expression to create an error *)
let sexp_example = Error.create "Unexpected character" 'c' Char.sexp_of_t

(* flatmap *)
let bind option ~f =
  match option with
  | None -> None
  | Some x -> f x
;;

(* using Option.bind *)
let compute_bounds ~compare list =
  let sorted = List.sort ~compare list in
  Option.bind (List.hd sorted) ~f:(fun first ->
    Option.bind (List.last sorted) ~f:(fun last -> Some (first, last)))
;;

(* using use monadic infix find  *)
let compute_bounds' ~compare list =
  let open Option.Monad_infix in
  let sorted = List.sort ~compare list in
  List.hd sorted >>= fun first -> List.last sorted >>= fun last -> Some (first, last)
;;

(* using ppx_let *)
let compute_bounds'' ~compare list =
  let open Option.Let_syntax in
  let sorted = List.sort ~compare list in
  (* feels like haskell let binding *)
  let%bind first = List.hd sorted in
  let%bind last = List.last sorted in
  Some (first, last)
;;

(* using option helper *)
let compute_bounds''' ~compare list =
  let sorted = List.sort ~compare list in
  Option.both (List.hd sorted) (List.last sorted)
;;

(* Data Serialization with S-Expressions *)

(* S-Expressions - nested, parenthetical expressions whose
   atomic values are strings
   (this (is an) (s expression)) *)

(* Basic Usage *)
module type My_sexp = sig
  type t =
    | Atom of string
    | List of t list
end

open Core;;

Sexp.List
  [ Sexp.Atom "this"
  ; Sexp.List [ Sexp.Atom "is"; Sexp.Atom "an" ]
  ; Sexp.List [ Sexp.Atom "s"; Sexp.Atom "expression" ]
  ]
;;

(* Core registers a pretty printer for Sexp with toplevel *)
Sexp.to_string (Sexp.List [ Sexp.Atom "1"; Sexp.Atom "2" ]);;
Sexp.of_string "(1 2 (3 4))";;

(* Base does not include Sexp.of_string to keep is light weight, use Parsexp.Single.parse_string_exn *)

(* Most types in Core and Base provide from an to sexp helpers *)
Int.sexp_of_t 3;;
String.sexp_of_t "hello";;
Exn.sexp_of_t (Invalid_argument "foo");;

(* also polymorphic containers provide helpers *)
List.sexp_of_t Int.sexp_of_t [ 1; 2; 3 ];;
List.t_of_sexp Int.t_of_sexp (Sexp.of_string "(1 2 3)")

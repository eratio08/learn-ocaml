open Base

(* GADTs *)
type value =
  | Int of int
  | Bool of bool

type expr =
  | Value of value
  | Eq of expr * expr
  | Plus of expr * expr
  | If of expr * expr * expr

exception Ill_typed

let rec eval expr =
  match expr with
  | Value v -> v
  | If (c, t, e) ->
    (match eval c with
     | Bool b -> if b then eval t else eval e
     | Int _ -> raise Ill_typed)
  | Eq (x, y) ->
    (match eval x, eval y with
     | Bool _, _ | _, Bool _ -> raise Ill_typed
     | Int f1, Int f2 -> Bool (f1 = f2))
  | Plus (x, y) ->
    (match eval x, eval y with
     | Bool _, _ | _, Bool _ -> raise Ill_typed
     | Int f1, Int f2 -> Int (f1 + f1))
;;

(* The ill-typed branches should be preventable using the type system *)
module type Typesafe_lang_sig = sig
  type 'a t

  (** function for constructing expressions *)

  val int : int -> int t
  val bool : bool -> bool t
  val if_ : bool t -> 'a t -> 'a t -> 'a t
  val eq : 'a t -> 'a t -> bool t
  val plus : int t -> int t -> int t

  (** Evaluation function *)

  val int_eval : int t -> int
  val bool_eval : bool t -> bool

  (* implementing a polymorphic eval is not possible here*)
end

module Typesafe_lang : Typesafe_lang_sig = struct
  type 'a t = expr

  let int x = Value (Int x)
  let bool x = Value (Bool x)
  let if_ c t e = If (c, t, e)
  let eq x y = Eq (x, y)
  let plus x y = Plus (x, y)

  let int_eval expr =
    match eval expr with
    | Int x -> x
    | Bool _ -> raise Ill_typed
  ;;

  let bool_eval expr =
    match eval expr with
    | Int _ -> raise Ill_typed
    | Bool b -> b
  ;;
end

(* The following can not be constructed, by using phantom types *)
(* let expr = Typesafe_lang.(plus (int 3) (bool false)) *)

(*
   Phantom types are types that like 'a here that is not used and work function
   as kind of marker.

   Opaque types are types that do not expose constructors,
   so can only be constructed by the module.
   The module will expose type-safe constructor functions for the types.
*)

(*
   using phantom types here did not solve the initial issue as ill-typed
   expressions can still be constructed

   Typesafe_lang.eq can still be constructed Ill_typed
*)

(* compiles and will raise *)
let expr = Typesafe_lang.(eq (bool true) (bool false))

type 'a value' =
  | Int of 'a
  | Bool of 'a

type 'a expr' =
  | Value of 'a value'
  | Eq of 'a expr' * 'a expr'
  | Plus of 'a expr' * 'a expr'
  | If of bool expr' * 'a expr' * 'a expr'

let i x = Value (Int x)
and b x = Value (Bool x)
and ( +: ) x y = Plus (x, y)
;;

(* All of these work as expected *)
i 3;;
b false;;
i 3 +: i 4;;

(* but also the following is allowed which is wrong*)
b 3

(* First GADT *)

(* Type constructors are typed like single argument functions *)
type _ value'' =
  | Int : int -> int value''
  | Bool : bool -> bool value''

type _ expr'' =
  | Value : 'a value'' -> 'a expr''
  | Eq : int expr'' * int expr'' -> bool expr''
  | Plus : int expr'' * int expr'' -> int expr''
  | If : bool expr'' * 'a expr'' * 'a expr'' -> 'a expr''

let i x = Value (Int x)
and b x = Value (Bool x)
and ( +: ) x y = Plus (x, y)
;;

i 3;;
b false;;
i 3 +: i 4

(* will fail due to the type system catching this illegal *)
(* i 3 +: b false;; *)

(* the extra type annotation is required if GADTs are used *)
let eval_value : type a. a value'' -> a = function
  | Int x -> x
  | Bool x -> x
;;

let rec eval : type a. a expr'' -> a = function
  | Value v -> eval_value v
  | If (c, t, e) -> if eval c then eval t else eval e
  | Eq (x, y) -> eval x = eval y
  | Plus (x, y) -> eval x + eval y
;;

(* work around the type annotation by adding a locally abstracted type *)
let eval_value (type a) (v : a value'') : a =
  match v with
  | Int x -> x
  | Bool x -> x
;;

(* let rec eval' (type a) (e : a expr'') : a = *)
(*   match e with *)
(*   | Value v -> eval_value' v *)
(*   | If (c, t, e) -> if eval' c then eval' t else eval' e *)
(*   | Eq  (x,y) -> eval' x = eval' y *)
(*   | Plus (x,y) -> eval' x + eval' y *)
(* ;; *)
(* Does not work as GADTs do not play well with recursion *)

(* this resolves is because a is not specialize to eval but to the nested lambda *)
let rec eval : 'a. 'a expr'' -> 'a =
  fun (type a) (x : a expr'') ->
  match x with
  | Value v -> eval_value v
  | If (c, t, e) -> if eval c then eval t else eval e
  | Eq (x, y) -> eval x = eval y
  | Plus (x, y) -> eval x + eval y
;;

(* the above can be simplified to *)
let rec eval : type a. a expr'' -> a = function
  | Value v -> eval_value v
  | If (c, t, e) -> if eval c then eval t else eval e
  | Eq (x, y) -> eval x = eval y
  | Plus (x, y) -> eval x + eval y
;;

(* Use in case of varying return types *)

(* implement a find that is configurable on the type how missing values are handled *)
module If_not_found = struct
  type 'a t =
    | Raise
    | Return_none
    | Default_to of 'a
end

let rec flexible_find list ~f (if_not_found : _ If_not_found.t) =
  match list with
  | hd :: tl -> if f hd then Some hd else flexible_find ~f tl if_not_found
  | [] ->
    (match if_not_found with
     | Raise -> failwith "Element not found"
     | Return_none -> None
     | Default_to x -> Some x)
;;

(* works but always returns an option, even when the Raise case is passed *)

(* using GADT, first type in the tuple is for the list, the other for the return type *)

module If_not_found' = struct
  type (_, _) t =
    | Raise : ('a, 'a) t
    | Return_none : ('a, 'a option) t
    | Default_to : 'a -> ('a, 'a) t
end

let rec flexible_find : type a b. f:(a -> bool) -> a list -> (a, b) If_not_found'.t -> b =
  fun ~f list if_not_found ->
  match list with
  | [] ->
    (match if_not_found with
     | Raise -> failwith "No matching item found"
     | Return_none -> None
     | Default_to x -> x)
  | hd :: tl ->
    if f hd
    then (
      match if_not_found with
      | Raise -> hd
      | Return_none -> Some hd
      | Default_to _ -> hd)
    else flexible_find ~f tl if_not_found
;;

flexible_find ~f:(fun x -> x > 10) [ 1; 2; 3 ] Return_none;;
flexible_find ~f:(fun x -> x > 10) [ 1; 2; 3 ] (Default_to 10)

(* flexible_find ~f:(fun x -> x > 10) [ 1; 2; 3 ] Raise *)

(* Capturing the Unknown *)

(* this code works with unknown types, these types are universally quantified i.e. for all *)
let tuple x y = x, y

(*
   to existentially quantified types we use GADTs,
   'a is existentially quantified as is only appears on the left-hand side
*)
type stringable =
  | Stringable :
      { value : 'a
      ; to_string : 'a -> string
      }
      -> stringable

let print_stringable (Stringable s) = Stdio.print_endline (s.to_string s.value)

let stringables =
  let s value to_string = Stringable { to_string; value } in
  [ s 100 Int.to_string; s 12.3 Float.to_string; s "foo" Fn.id ]
;;

List.iter ~f:print_stringable stringables

(* due to the only left-hand appearance the following is not possible, is the type can no escape *)
(* let get_value (Stringable s) = s.value;; *)

(* the error message show a `$` before the type which signals it is existential *)

(* Abstracting Computational Machines *)

(* GADTs are used to write combinators *)

module type Pipeline = sig
  type ('input, 'output) t

  val ( @> ) : ('a -> 'b) -> ('b, 'c) t -> ('a, 'c) t
  val empty : ('a, 'a) t
end

(* functor *)
module Example_pipeline (Pipeline : Pipeline) = struct
  open Pipeline

  let sum_files_size =
    (fun () -> Sys_unix.ls_dir ".")
    @> List.filter ~f:Sys_unix.is_file_exn
    @> List.map ~f:(fun file_name -> (Unix.lstat file_name).st_size)
    @> List.sum (module Int) ~f:Int.to_int_exn
    @> empty
  ;;
end

module Basic_pipeline : sig
  include Pipeline

  val exec : ('a, 'b) t -> 'a -> 'b
end = struct
  type ('input, 'output) t = 'input -> 'output

  let empty = Fn.id
  let ( @> ) f t input = t (f input)
  let exec t input = t input
end

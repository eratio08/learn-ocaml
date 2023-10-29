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
  open Core_unix

  let sum_files_size =
    (fun () -> Sys_unix.ls_dir ".")
    @> List.filter ~f:Sys_unix.is_file_exn
    @> List.map ~f:(fun file_name -> (Core_unix.lstat file_name).st_size)
    @> List.sum (module Int) ~f:Int64.to_int_exn
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

(* implement as GADTs *)
type (_, _) pipeline =
  | Step : ('a -> 'b) * ('b, 'c) pipeline -> ('a, 'c) pipeline
  | Empty : ('a, 'a) pipeline

let rec exec : type a b. (a, b) pipeline -> a -> b =
  fun pipeline input ->
  match pipeline with
  | Empty -> input
  | Step (f, tail) -> exec tail (f input)
;;

let exec_with_profile pipeline input =
  let rec loop
    : type a b.
      (a, b) pipeline -> a -> Time_ns_unix.Span.t list -> b * Time_ns_unix.Span.t list
    =
    fun pipeline input rev_profile ->
    match pipeline with
    | Empty -> input, rev_profile
    | Step (f, tail) ->
      let start = Time_ns_unix.now () in
      let output = f input in
      let elapsed = Time_ns_unix.diff (Time_ns_unix.now ()) start in
      loop tail output (elapsed :: rev_profile)
  in
  let output, rev_profile = loop pipeline input [] in
  output, List.rev rev_profile
;;

(* Narrowing the Possibilities *)

(* with out GADT the state might be modeled as following *)
module User_name = struct
  type t = Name of string
end

module User_id = struct
  type t = Id of int
end

module Permissions = struct
  type t =
    | Read
    | Write

  let check permissions user_id = true
end

type logon_request =
  { user_name : User_name.t
  ; user_id : User_id.t option
  ; permissions : Permissions.t option
  }

let authorize request =
  match request.user_id, request.permissions with
  | None, _ | _, None -> Error "Can't check authorization: data incomplete"
  | Some user_id, Some permissions -> Ok (Permissions.check permissions user_id)
;;

(* A Completion-Sensitive Option Type *)
(* these types will be used as marker *)
type incomplete = Incomplete
type complete = Complete

type (_, _) coption =
  | Absent : (_, incomplete) coption
  | Present : 'a -> ('a, _) coption

let get ~default o =
  match o with
  | Present x -> x
  | Absent -> default
;;

let get' (o : (_, complete) coption) =
  match o with
  | Present x -> x
;;

let get'' (Present x : (_, complete) coption) = x

(* A Completion-Sensitive Request Type *)
type 'c logon_request' =
  { user_name : User_name.t
  ; user_id : (User_id.t, 'c) coption
  ; permissions : (Permissions.t, 'c) coption
  }

let set_user_id request x = { request with user_id = Present x }
let set_permissions request x = { request with permissions = Present x }

(*
   setting fields will not complete the type so the following will

   using type annotation ensure the return type is not generic put complete
*)
let check_completeness request : complete logon_request' option =
  match request.user_id, request.permissions with
  | Absent, _ | _, Absent -> None
  | (Present _ as user_id), (Present _ as permissions) ->
    Some { request with user_id; permissions }
;;

(* will only work on completed coptionals *)
let authorize (request : complete logon_request') =
  let { user_id = Present user_id; permissions = Present permissions; _ } = request in
  Permissions.check permissions user_id
;;

(*Type distinctness and Abstraction *)

(*
   the two marker types do not need to have a different constructor for
   types to be distinct, variants are nominal
*)
type incomplete' = Z
type complete' = Z

let i = (Z : incomplete')
and c = (Z : complete')

(* the following is invalid, as they are not the same type *)
(* [ i; c ] *)

type ('a, _) coption' =
  | Absent : (_, incomplete) coption'
  | Present : 'a -> ('a, _) coption'

let assume_complete (coption : (_, complete') coption') =
  match coption with
  | Present x -> x
;;

(* not exposing the variants will cause a problem *)
module M : sig
  type incomplete
  type complete
end = struct
  type incomplete = Z
  type complete = Z
end

type ('a, _) coption'' =
  | Absent : (_, M.incomplete) coption''
  | Present : 'a -> ('a, _) coption''

(*
   the match is not exhaustive any more, because the
   underlying types are hidden from the type system
*)

(* let assume_complete' (coption : (_, M.complete) coption') = *)
(*   match coption with *)
(*   | Present x -> x *)
(* ;; *)

(* exposing the types in the signature might help*)
module M' : sig
  type incomplete = Z
  type complete = Z
end = struct
  type incomplete = Z
  type complete = Z
end

type ('a, _) coption''' =
  | Absent : (_, M'.incomplete) coption'''
  | Present : 'a -> ('a, _) coption'''

(* still no luck, it's still not exhaustive *)

(* let assume_complete' (coption : (_, M'.complete) coption'') = *)
(*   match coption with *)
(*   | Present x -> x *)
(* ;; *)

(* to be exhaustive the types need to be definitely different
   as shown in the following, the types might be the same internally *)

module M''' : sig
  type incomplete = Z
  type complete = Z
end = struct
  type incomplete = Z
  type complete = incomplete = Z
end

(* Narrowing Without GADTs *)

(* narrowing can also be done via the uninhabited type
   it's a type with an associated value *)

(* like Base.Nothing.t *)
type nothing = |

open Stdio

let print_result (x : (int, string) Result.t) =
  match x with
  | Ok x -> printf "%d\n" x
  | Error x -> printf "ERROR: %s\n" x
;;

(* can not print x of nothing in the error case*)

(* let print_result' (x : (int, Nothing.t) Result.t) = *)
(*   match x with *)
(*   | Ok x -> printf "%d\n" x *)
(*   | Error x -> printf "ERROR: %s\n" x *)
(* ;; *)

(* still a warning because the compiler know that the error case can not be reached *)
(* let print_result' (x : (int, Nothing.t) Result.t) = *)
(*   match x with *)
(*   | Ok x -> printf "%d\n" x *)
(*   | Error _ -> printf "ERROR\n" *)
(* ;; *)

(* using `.` to mark the refutation case *)
let print_result' (x : (int, Nothing.t) Result.t) =
  match x with
  | Ok x -> printf "%d\n" x
  | Error _ -> .
;;

(* as the case can not be reached, it can be omitted *)
let print_result' (x : (int, Nothing.t) Result.t) =
  match x with
  | Ok x -> printf "%d\n" x
;;

(* narrowing can be useful for highly configurable libraries
   to select specific modes
   the following will demonstrate this using async rpc module
*)
open Core
open Async

let rpc =
  Rpc.State_rpc.create
    ~name:"int-map"
    ~version:1
    ~bin_query:[%bin_type_class: unit]
    ~bin_state:[%bin_type_class: int Map.M(String).t]
    ~bin_update:[%bin_type_class: int Map.M(String).t]
    ~bin_error:[%bin_type_class: unit]
    ()
;;

(* just a stub *)
let handle_state_changes s u = Deferred.unit

(*
   the error and query type are unit here and the type system will ban this type
   Error handling is
*)
let dispatch conn =
  match%bind Rpc.State_rpc.dispatch rpc conn () >>| ok_exn with
  | Ok (initial_state, updates, _) -> handle_state_changes initial_state updates
  | Error () -> Deferred.unit
;;

(* Limitations of GADTs *)
(* Or-Patterns *)
module Host_and_port = struct
  type t =
    { host : string
    ; port : int
    }

  let sexp_of_t t = Sexp.Atom "dummy"
end

module Source_kind = struct
  type _ t =
    | Filename : string t
    | Host_and_port : Host_and_port.t t
    | Raw_data : string t
end

let source_to_sexp (type a) (kind : a Source_kind.t) (source : a) : Sexp.t =
  match kind with
  | Filename -> String.sexp_of_t source
  | Host_and_port -> Host_and_port.sexp_of_t source
  | Raw_data -> String.sexp_of_t source
;;

(* the code for Filename and Raw_data is the same but trying to merge these
   using `|` (or-pattern)  will fail here as the type information that
   is discovered during the pattern match is used

   if this information is not used it can work e.g. in the following
*)

let requires_io (type a) (kind : a Source_kind.t) =
  match kind with
  | Filename | Host_and_port -> true
  | Raw_data -> false
;;

(* Deriving Serializers *)

(* to generate s-expressions the pre-processor `ppx_sexp_value` is used *)
type position =
  { x : float
  ; y : float
  }
  [@@deriving sexp];;

sexp_of_position { x = 3.5; y = -2. };;
position_of_sexp (Sexp.of_string "((x 72) (y 1.2)")

(* this `sexp` does not work well with GADTs
   number_kind_of_sexp cannot be expresses with the ocaml type system
*)
type _ number_kind =
  | Int : int number_kind
  | Float : float number_kind
[@@deriving sexp_of]

(* writing a deserialiser is hard but possible *)
type packed_number_kind = P : _ number_kind -> packed_number_kind

type simple_number_kind =
  | Int
  | Float
[@@deriving of_sexp]

let simple_number_kind_of_packed_number_kind kind : packed_number_kind =
  match kind with
  | Int -> P Int
  | Float -> P Float
;;

let number_kind_of_sexp sexp =
  simple_number_kind_of_sexp sexp |> simple_number_kind_of_packed_number_kind
;;

List.map ~f:number_kind_of_sexp [ Sexp.of_string "Float"; Sexp.of_string "Int" ]

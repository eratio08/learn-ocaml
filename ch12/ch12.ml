(* First-Class Modules *)

(* firs-class modules are modules that are ordinary values
   that can be converted to a module and be converted back to a value

   this kinda moves modules into the core language
*)

(* Working with First-Class Modules *)

(* Creating First-Class Modules *)

open Base

module type X_int = sig
  val x : int
end

module Three : X_int = struct
  let x = 3
end

(* firs-class modules is created with
   (module <Module> : <Module_type>)
*)

let three = (module Three : X_int)

(* Inference and Anonymous Modules *)
module Four = struct
  let x = 4
end

(* the module type signature is not required if it can be inferred *)
let numbers = [ three; (module Four) ]

(* using anonymous block is also allowed *)
let numbers' =
  [ three
  ; (module struct
      let x = 4
    end)
  ]
;;

(* Unpack First-Class Modules *)

(* unpack with
   (val <first_class_modules> : <Module_type>)
*)

module New_three = (val three : X_int);;

New_three.x

(* Functions for Manipulating First-Class Modules *)

(* turn first class module to module and access internal value *)
let to_int m =
  let module M = (val m : X_int) in
  M.x
;;

(* add two first class module to each other *)
let plus m1 m2 =
  (module struct
    let x = to_int m1 + to_int m2
  end : X_int)
;;

(* unpacking can be done with pattern matching *)
let to_int' (module M : X_int) = M.x
let six = plus three three;;

to_int (List.fold ~init:six ~f:plus [ three; three ])

(* Richer First-Class Modules *)
module type Bumpable = sig
  type t

  val bump : t -> t
end

module Int_bumper = struct
  type t = int

  let bump b = b + 1
end

module Float_bumper = struct
  type t = float

  let bump n = n +. 1.
end

let int_bumper = (module Int_bumper : Bumpable)

(* Exposing types *)

(* int_bumper is fully abstract so to make is useful Bumper.t needs to be
   expose to be of type int *)
(* let (module Bumper) = int_bumper in *)
(* Bumper.bump 3 *)

(* using sharing constraints *)
let int_bumper = (module Int_bumper : Bumpable with type t = int)
let float_bumper = (module Float_bumper : Bumpable with type t = float);;

let (module Bumper) = int_bumper in
Bumper.bump 3
;;

let (module Bumper) = float_bumper in
Bumper.bump 3.5

(* first-class modules can be used polymorphically

   here a is locally abstract type
   a is a pseudo parameter, it introduces a new type a, which is abstract and part of the sharing constraint
*)
let bump_list (type a) (module Bumper : Bumpable with type t = a) (l : a list) =
  List.map ~f:Bumper.bump l
;;

bump_list int_bumper [ 1; 2; 3 ];;
bump_list float_bumper [ 1.5; 2.5; 3.5 ]

(* More on Locally Abstract Types *)

(* locally abstract types are abstract type in the function
   but polymorphic outside of the function *)
let wrap_in_list (type a) (x : a) = [ x ]

(* the inferred type is generic, but a is abstract in side
   trying to use a as if it was a concert type will fail *)

(* let double_int (type a) (x:a) = x + x;; *)

(* c common use case of local abstract types - first-class module constructors *)
module type Comparable = sig
  type t

  val compare : t -> t -> int
end

let create_comparable (type a) compare =
  (module struct
    type t = a

    let compare = compare
  end : Comparable
    with type t = a)
;;

create_comparable Int.compare;;
create_comparable Float.compare

(* the modules can be passed to functors *)

(* Example: A Query-Handling Framework *)

module type Query_handler = sig
  (** Configuration for a query handler *)
  type config

  val sexp_of_config : config -> Sexp.t
  val config_of_sexp : Sexp.t -> config

  (** The name of the query-handling service *)
  val name : string

  (** The state of the query handler *)
  type t

  (** Creates a new query handler from a config *)
  val create : config -> t

  (** Evaluate a given query, where both input and output are s-expressions *)
  val eval : t -> Sexp.t -> Sexp.t Or_error.t
end

(* deriving can also be applied to modules *)
module type M = sig
  type t [@@deriving sexp]
end

(* Implementing a Query Handler *)

module Unique = struct
  type config = int [@@deriving sexp]
  type t = { mutable next_id : int }

  let name = "unique"
  let create start_at = { next_id = start_at }

  let eval t sexp =
    match Or_error.try_with (fun () -> unit_of_sexp sexp) with
    | Error _ as err -> err
    | Ok () ->
      let response = Ok (Int.sexp_of_t t.next_id) in
      t.next_id <- t.next_id + 1;
      response
  ;;
end

(* create an instance *)
let unique = Unique.create 0;;

Unique.eval unique (Sexp.List []) (* Ok 0 *);;
Unique.eval unique (Sexp.List []) (* Ok 1 *)

(* another implementation *)

module List_dir = struct
  type config = string [@@deriving sexp]
  type t = { cwd : string }

  (** [is abs p] Returns true if [p] is an absolute path *)
  let is_abs p = String.length p > 0 && Char.( = ) p.[0] '/'

  let name = "ls"
  let create cwd = { cwd }

  let eval t sexp =
    match Or_error.try_with (fun () -> string_of_sexp sexp) with
    | Error _ as err -> err
    | Ok dir ->
      let dir = if is_abs dir then dir else Core.Filename.concat t.cwd dir in
      Ok (Array.sexp_of_t String.sexp_of_t (Sys_unix.readdir dir))
  ;;
end

let list_dir = List_dir.create "/var";;

List_dir.eval list_dir (sexp_of_string ".")

(* Dispatch to Multiple Query Handlers *)

(* a module combining a query handler module and an instance of it *)
module type Query_handler_instance = sig
  (* module *)
  module Query_handler : Query_handler

  (* instance *)
  val this : Query_handler.t
end

(* instance creation *)
let unique_instance =
  (module struct
    module Query_handler = Unique

    let this = Unique.create 0
  end : Query_handler_instance)
;;

(* this can be shortened to *)
let build_instance (type a) (module Q : Query_handler with type config = a) config =
  (module struct
    module Query_handler = Q

    let this = Q.create config
  end : Query_handler_instance)
;;

let unique_instance' = build_instance (module Unique) 0
let list_dir_instance = build_instance (module List_dir) "/var"

(* goal is to have a function to dispatch any query *)
(* first build a dispatch table *)
let build_dispatch_table_handler handlers =
  let table = Hashtbl.create (module String) in
  List.iter handlers ~f:(fun ((module I : Query_handler_instance) as instance) ->
    Hashtbl.set table ~key:I.Query_handler.name ~data:instance);
  table
;;

(* dispatcher *)
let dispatch dispatch_table name_and_query =
  match name_and_query with
  | Sexp.List [ Sexp.Atom name; query ] ->
    (match Hashtbl.find dispatch_table name with
     | None -> Or_error.error "Could not find matching handler" name String.sexp_of_t
     | Some (module I : Query_handler_instance) -> I.Query_handler.eval I.this query)
  | _ -> Or_error.error_string "malformed query"
;;

(* Functors *)

(* Functor are functions from modules to modules

   they solve dependency injection, autoextension of modules
   and instantiation of modules with state
*)

(* A Trivial Example *)
open Base

(* interface *)
module type X_int = sig
  val x : int
end

(* functor return types are optional *)
module Increment (M : X_int) : X_int = struct
  let x = M.x + 1
end

(* implementation *)
module Three = struct
  let x = 3
end

(* functors can be used to define new modules *)
module Four = Increment (Three)

(* this module still satisfies the X_int interface *)
module Three_and_more = struct
  let x = 3
  let y = "three"
end

module Four' = Increment (Three_and_more)

(* A Bigger Example: Computing with Intervals *)

module type Comparable = sig
  type t

  val compare : t -> t -> int
end

module Make_interval (Endpoint : Comparable) = struct
  type t =
    | Interval of Endpoint.t * Endpoint.t
    | Empty

  (** [create low high] creates a new inerval from [low] to [heigh].
      If [low > heigh], the interval is empty *)
  let create low heigh =
    if Endpoint.compare low heigh > 0 then Empty else Interval (low, heigh)
  ;;

  (** Returns true of the interval is empty *)
  let is_empty = function
    | Empty -> true
    | Interval _ -> false
  ;;

  (** [contains t x] returns true if [x] is contained in the interval [t] *)
  let contains t x =
    match t with
    | Empty -> false
    | Interval (l, h) -> Endpoint.compare x l >= 0 && Endpoint.compare x h <= 0
  ;;

  (** [intersect t1 t2] return the intersection of the two input intervals *)
  let intersect t1 t2 =
    let min x y = if Endpoint.compare x y <= 0 then x else y in
    let max x y = if Endpoint.compare x y >= 0 then x else y in
    match t1, t2 with
    | Empty, _ | _, Empty -> Empty
    | Interval (l1, h1), Interval (l2, h2) -> create (max l1 l2) (min h1 h2)
  ;;
end

(* instantiate the functor for ints *)
module Int_interval = Make_interval (struct
    type t = int

    let compare = Int.compare
  end)

(* the interface of the functor matches core so int can be used directly *)
module Int_interval' = Make_interval (Int)
module String_interval = Make_interval (String)

let i1 = Int_interval.create 3 8
let i2 = Int_interval.create 4 10;;

Int_interval.intersect i1 i2

(* this design allows to use any comparison function *)
module Rev_int_interval = Make_interval (struct
    type t = int

    let compare x y = Int.compare y x
  end)

(* will be empty *)
let interval = Int_interval.create 4 3

(* [4,3] *)
let rev_interval = Rev_int_interval.create 4 3;;

(* The type system will prevent cross type usage *)
(* Int_interval.contains rev_interval 3;; *)

(* Make the Functor Abstract *)

(* the current implementation of the interval strictly requires
   lower < high
   this is only enforce by the create function and can be bypased
   because Int_interval.t is not abstract
*)

Int_interval.is_empty (* using create *) (Int_interval.create 4 3);;
Int_interval.is_empty (* bypassing create *) (Int_interval.Interval (4, 3))

(* to prevent this the output of Make_interval needs to be restricted *)
module type Interval_intf = sig
  type t
  type endpoint

  val create : endpoint -> endpoint -> t
  val is_empty : t -> bool
  val contains : t -> endpoint -> bool
  val intersect : t -> t -> t
end

module Make_interval' (Endpoint : Comparable) : Interval_intf = struct
  type endpoint = Endpoint.t

  type t =
    | Interval of Endpoint.t * Endpoint.t
    | Empty

  (** [create low high] creates a new inerval from [low] to [heigh].
      If [low > heigh], the interval is empty *)
  let create low heigh =
    if Endpoint.compare low heigh > 0 then Empty else Interval (low, heigh)
  ;;

  (** Returns true of the interval is empty *)
  let is_empty = function
    | Empty -> true
    | Interval _ -> false
  ;;

  (** [contains t x] returns true if [x] is contained in the interval [t] *)
  let contains t x =
    match t with
    | Empty -> false
    | Interval (l, h) -> Endpoint.compare x l >= 0 && Endpoint.compare x h <= 0
  ;;

  (** [intersect t1 t2] return the intersection of the two input intervals *)
  let intersect t1 t2 =
    let min x y = if Endpoint.compare x y <= 0 then x else y in
    let max x y = if Endpoint.compare x y >= 0 then x else y in
    match t1, t2 with
    | Empty, _ | _, Empty -> Empty
    | Interval (l1, h1), Interval (l2, h2) -> create (max l1 l2) (min h1 h2)
  ;;
end

(* Sharing Constraints *)

(* the module is too abstract, it can't be constructed *)
module Int_interval'' = Make_interval' (Int)

(* create will not work as Int.t needs to equal Endpoint.t *)
(* Int_interval''.create 3 4;; *)

(* this is archived by sharing constraints

   <Module_type> with type <type> = <type'>

   multiple constraints can be shared by

   <Module_type> with type <type> = <type1'> and type <type2> = <type2'>
*)

module type Int_interval_intf = Interval_intf with type endpoint = int

(* Int_interval''.create 3 4;; *)

(* sharing constraints can we used in functors *)
module Make_interval'' (Endpoint : Comparable) :
  Interval_intf with type endpoint = Endpoint.t = struct
  type endpoint = Endpoint.t

  type t =
    | Interval of Endpoint.t * Endpoint.t
    | Empty

  (** [create low high] creates a new inerval from [low] to [heigh].
      If [low > heigh], the interval is empty *)
  let create low heigh =
    if Endpoint.compare low heigh > 0 then Empty else Interval (low, heigh)
  ;;

  (** Returns true of the interval is empty *)
  let is_empty = function
    | Empty -> true
    | Interval _ -> false
  ;;

  (** [contains t x] returns true if [x] is contained in the interval [t] *)
  let contains t x =
    match t with
    | Empty -> false
    | Interval (l, h) -> Endpoint.compare x l >= 0 && Endpoint.compare x h <= 0
  ;;

  (** [intersect t1 t2] return the intersection of the two input intervals *)
  let intersect t1 t2 =
    let min x y = if Endpoint.compare x y <= 0 then x else y in
    let max x y = if Endpoint.compare x y >= 0 then x else y in
    match t1, t2 with
    | Empty, _ | _, Empty -> Empty
    | Interval (l1, h1), Interval (l2, h2) -> create (max l1 l2) (min h1 h2)
  ;;
end

module Int_interval''' = Make_interval'' (Int)

let i = Int_interval'''.create 3 4;;

Int_interval'''.contains i 5

(* Destructive Substitution *)

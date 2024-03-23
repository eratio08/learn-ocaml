(** Imperative Programming *)
open Base

open Stdio

(* primitive mutable data in ocaml is possible with mutable field of records and arrays (and some others) *)

(* array *)
let arr = [| 1; 2; 3 |]

(* bytes & strings *)
let b = Bytes.of_string "foobar";;

(* strings can be imagines as 8bit char arrays that is immutable *)
Bytes.set b 0 (Char.uppercase (Bytes.get b 0));;

(* returns Foobar *)
Bytes.to_string b

(*
   Bigarrays is a handle to a block of memory stored outside of OCaml heap.
   Is mostly usable to interact with C or Fortran.
*)

(*
   mutable records fields are mutated by using bytes
   `record.field <- expr`
   is use
*)
type mutable_record = { mutable m : int }

let x = { m = 1 };;

x.m <- 2

(* ref cells is a construct using mutable records to implement mutability *)

(*
   type 'a ref = { mutable contents : 'a }
   let ( ! ) r = r.contents
   let ( := ) r = r.contents
*)

(* construct the cell *)
let x = ref 1;;

(* to access the value in x *)
!x;;

(* to mutate the value in ref *)
x := !x + 1;;

(* will return 2 *)
!x

(* FFI are another mutation point *)

(*
   for loops from 0 to 3 (inclusive)
   the loop variable is immutable in the scope of the loop
*)
let for_loop _ =
  for i = 0 to 3 do
    printf "i = %d\n" i
  done
;;

let for_loop_downto _ =
  for i = 3 downto 0 do
    printf "i = %d\n" i
  done
;;

(* a while loop, here to implement in place array inversion *)
let rev_inplace ar =
  let i = ref 0 in
  let j = ref (Array.length ar - 1) in
  while !i < !j do
    let tmp = ar.(!i) in
    ar.(!i) <- ar.(!j);
    ar.(!j) <- tmp;
    Int.incr i;
    Int.decr j
  done
;;

(* benign effects := side effect that improve performance *)

(* laziness, turns expression in to lazy value that is evaluated only if needed *)
let v =
  lazy
    (print_endline "performing lazy computation";
     Float.sqrt 16.)
;;

Lazy.force v;;

(* print only happen once *)
Lazy.force v

type 'a lazy_state =
  | Delayed of (unit -> 'a)
  | Value of 'a
  | Exn of exn

type 'a our_lazy = { mutable state : 'a lazy_state }

let our_lazy f = { state = Delayed f }

let v =
  our_lazy (fun () ->
    print_endline "performing lazy computation";
    Float.sqrt 16.)
;;

let our_force l =
  match l.state with
  | Value x -> x
  | Exn e -> raise e
  | Delayed f ->
    (try
       let x = f () in
       l.state <- Value x;
       x
     with
     | exn ->
       l.state <- Exn exn;
       raise exn)
;;

our_force v;;
our_force v

(* Memoization *)
(* will leak memory, as it memoized for erver*)
let memoize m f =
  let memo_table = Hashtbl.create m in
  fun x -> Hashtbl.find_or_add memo_table x ~default:(fun () -> f x)
;;

let rec edit_distance s t =
  match String.length s, String.length t with
  | 0, x | x, 0 -> x
  | len_s, len_t ->
    let s' = String.drop_suffix s 1 in
    let t' = String.drop_suffix t 1 in
    let cost_to_drop_both = if Char.( = ) s.[len_s - 1] t.[len_t - 1] then 0 else 1 in
    List.reduce_exn
      ~f:Int.min
      [ edit_distance s' t + 1
      ; edit_distance s t' + 1
      ; edit_distance s' t' + cost_to_drop_both
      ]
;;

let time f =
  let open Core in
  let start = Time_float.now () in
  let x = f () in
  let stop = Time_float.now () in
  printf "Time: %F ms\n" (Time_float.diff stop start |> Time_float.Span.to_ms);
  x
;;

time (fun () -> edit_distance "OCaml" "ocaml")

(* time (fun () -> edit_distance "OCaml 4.13" "ocaml 4.13") *)

let rec fib i = if i <= 1 then i else fib (i - 1) + fib (i - 2)
let fib' = memoize (module Int) fib;;

time (fun () -> fib' 20)

(* time (fun () -> fib' 40);; *)

(* will be quick as the results is memoized *)
(* time (fun () -> fib' 40) *)

(* no recursion, just takes a fib function and sums the results *)
let fib_norec fib i = if i <= 1 then i else fib (i - 1) + fib (i - 2)
let rec fib'' i = fib_norec fib'' i;;

time (fun () -> fib'' 20)

(* time (fun () -> fib'' 40) *)

(* a polymorphic version of fib'' that can be any same shape non recursive function and make it recursive *)
let make_rec f_norec =
  let rec f x = f_norec f x in
  f
;;

let fib''' = make_rec fib_norec;;

time (fun () -> fib''' 20)

(* time (fun () -> fib''' 40) *)

(* make recursion memoize *)
let memo_rec m f_norec x =
  (*
     this implements the recursion with out using rec,
     using `let rec f = memoize m (fun x -> f_norec f x)` is not allowed
     due to the strict evaluation of ocaml.
     This is similar to the expression `let rec x = x + 1` which would be illegal
     as well, which can not be compiled as it would lead to an infinite loop.
     `x` is a simple value and not a function here.
     Only functions definition, constructors or lazy keyword are allowed on the right side of a let rec.
     This issue does not show up in haskell as it's lazy by default.
     Similar to this `let rec x = lazy (force x + 1);;` would be valid.
     Evaluation would fail though.
  *)
  let fref = ref (fun _ -> assert false) in
  (* here we call f_norec with f x which is stored in fref in the next line, effectively recursion *)
  let f = memoize m (fun x -> f_norec !fref x) in
  fref := f;
  f x
;;

(* make memoizing fib, effectively implement dynamic programming
   the trick to not leak memory is that the call if memo_rec does not call memoize directly,
   only the call to fib will.
   As a result the memo_table will be collected after fib terminates.
*)
let fib = memo_rec (module Int) fib_norec;;

time (fun () -> fib 20);;
time (fun () -> fib 40)

(* make optimize edit distance *)

(* add string pair module to make this a single argument to edit_distance *)
module String_pair = struct
  type t = string * string [@@deriving sexp_of, hash, compare]
end

let edit_distance' =
  memo_rec
    (module String_pair)
    (fun edit_distance' (s, t) ->
      match String.length s, String.length t with
      | 0, x | x, 0 -> x
      | len_s, len_t ->
        let s' = String.drop_suffix s 1 in
        let t' = String.drop_suffix t 1 in
        let cost_to_drop_both = if Char.( = ) s.[len_s - 1] t.[len_t - 1] then 0 else 1 in
        List.reduce_exn
          ~f:Int.min
          [ edit_distance' (s', t) + 1
          ; edit_distance' (s, t') + 1
          ; edit_distance' (s', t') + cost_to_drop_both
          ])
;;

time (fun () -> edit_distance' ("OCaml", "ocaml"));;
time (fun () -> edit_distance' ("OCaml 4.13", "ocaml 4.13"))

(* implement mem_rec without mutation using lazy *)
let lazy_memo_rec m f_norec x =
  let rec f = lazy (memoize m (fun x -> f_norec (force f) x)) in
  (force f) x
;;

(* IO *)

(* when using `printf` including `%!` will flush the stream *)

let create_number_file filename numbers =
  let outc = Out_channel.create filename in
  List.iter numbers ~f:(fun x -> Out_channel.fprintf outc "%d\n" x);
  Out_channel.close outc
;;

let sum_file filename =
  let file = In_channel.create filename in
  let numbers = List.map ~f:Int.of_string (In_channel.input_lines file) in
  let sum = List.fold ~init:0 ~f:( + ) numbers in
  In_channel.close file;
  sum
;;

create_number_file "numbers.txt" [ 1; 2; 3; 4; 5 ];;
sum_file "numbers.txt"

(* use protect to close the channel even in case of an error *)
let sum_file' filename =
  let file = In_channel.create filename in
  Exn.protect
    ~f:(fun () ->
      let numbers = List.map ~f:Int.of_string (In_channel.input_lines file) in
      List.fold ~init:0 ~f:( + ) numbers)
    ~finally:(fun () -> In_channel.close file)
;;

(* `with_file` handles file closing implicitly *)
let sum_file'' filename =
  In_channel.with_file filename ~f:(fun file ->
    let numbers = List.map ~f:Int.of_string (In_channel.input_lines file) in
    List.fold ~init:0 ~f:( + ) numbers)
;;

(* Order of Evaluation *)

let x = Float.sin 120. in
let y = Float.sin 75. in
let z = Float.sin 128. in
List.exists ~f:(fun x -> Float.O.(x < 0.)) [ x; y; z ]
;;

(* skips the evaluation of sin 128., as `force` will never be called on z' *)
let x' = lazy (Float.sin 120.) in
let y' = lazy (Float.sin 75.) in
let z' = lazy (Float.sin 128.) in
List.exists ~f:(fun x -> Float.O.(Lazy.force x < 0.)) [ x'; y'; z' ]
;;

(*
   the evaluation order within an expression is not defined by the spec,
   and it's defined by the compiler implementation

   as there is only one compiler for ocaml, it's behavior is the defacto
   standard

   the following array is evaluated backwards,
   to enforce another order use let bindings
*)
List.exists
  ~f:(fun x -> Float.O.(x < 0.))
  [ (printf "1\n";
     Float.sin 120.)
  ; (printf "2\n";
     Float.sin 75.)
  ; (printf "3\n";
     Float.sin 128.)
  ]

(* Side Effects and Weak Polymorphism *)

(*
   has only weak polymorphism, can only be bound to a single type
   indicated by the 'weak in the type signature
*)
let remember =
  let cache = ref None in
  fun x ->
    match !cache with
    | Some y -> y
    | None ->
      cache := Some x;
      x
;;

(* has real polymorphism *)
let identity x = x;;

(* The Value Restriction *)
(*
   weak polymorphism is inferred in case an unknown type is stored in a
   persistent mutable cell

   the value restriction, is a rule that determines when real polymorphism
   is inferred. e.g. a simple values like
   * constants
   * constructors that only contain simple values
   * function declarations
   * let bindings e.g. let var = expr1 in expr2 and both expr are simple values
*)

(* is a simple value, so will have type 'a -> 'a list *)
fun x -> [ x; x ];;

(*
   not a simple value, 'weak -> 'weak list
   OCamls type system does not differentiates between pure and impure functions

   There is no persistent mutable state in this case but still its only weak polymorphism
*)
identity (fun x -> [ x; x ])

(*
   Produces a new ref on each call, so no mutable persistent state in this functions
   thus this is strong polymorphic
*)
let f () = ref None

(* Partial Application and the Value Restriction *)

(* partially applied functions are not simple values *)

(* list of strings *)
let string_int_list = List.init 10 ~f:Int.to_string

(* weak polymorphic, it's weak because might create a persistent ref *)
let list_init_10 = List.init 10

(*
   not partially applied anymore and now strong polymorphic

   this is name 'eta expansion' and resolves these problems with the value restriction
*)
let list_init_1 ~f = List.init 10 ~f;;

(* Relaxing the Value Restriction *)

(* type is immutable so full polymorphic *)
identity [];;

(* type is mutable so weak polymorphic *)
identity [||]

(* Immutable concat list implementation *)
module Concat_list : sig
  type 'a t

  val empty : 'a t
  val singleton : 'a -> 'a t
  val concat : 'a t -> 'a t -> 'a t (* constant time *)
  val to_list : 'a t -> 'a list (* linear time *)
end = struct
  type 'a t =
    | Empty
    | Singleton of 'a
    | Concat of 'a t * 'a t

  let empty = Empty
  let singleton x = Singleton x
  let concat x y = Concat (x, y)

  let rec to_list_with_tail t tail =
    match t with
    | Empty -> tail
    | Singleton x -> x :: tail
    | Concat (x, y) -> to_list_with_tail x (to_list_with_tail y tail)
  ;;

  let to_list t = to_list_with_tail t []
end
;;

identity Concat_list.empty
(*
   Due to the value restriction ocaml treats it as mutable.
   This is caused by the signature, that obscures the immutability.
   Exposing the implementation in in the `mli` would resolve this.

   Turning the type variable into a covariant would also work.
   Turning `'a` into `+'a`, will make it full polymorphic.
*)

module Concat_list' : sig
  type +'a t

  val empty : 'a t
  val singleton : 'a -> 'a t
  val concat : 'a t -> 'a t -> 'a t (* constant time *)
  val to_list : 'a t -> 'a list (* linear time *)
end = struct
  type 'a t =
    | Empty
    | Singleton of 'a
    | Concat of 'a t * 'a t

  let empty = Empty
  let singleton x = Singleton x
  let concat x y = Concat (x, y)

  let rec to_list_with_tail t tail =
    match t with
    | Empty -> tail
    | Singleton x -> x :: tail
    | Concat (x, y) -> to_list_with_tail x (to_list_with_tail y tail)
  ;;

  let to_list t = to_list_with_tail t []
end

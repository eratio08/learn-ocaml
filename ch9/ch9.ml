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

(* laziness *)

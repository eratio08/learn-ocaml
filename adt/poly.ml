open Base

(* declaration without type keyword *)
let three = `Int 3
let four = `Float 4.0
let nan = `Not_a_number

(* > means type Float | Int and more *)
let example = [ three; four; nan ]

(* will show < as the type will be Float | Int and less *)
let is_positive = function
  | `Int x -> x > 0
  (* Local open of the Float module using Float.() *)
  | `Float x -> Float.(x > 0.)
;;

(* < and > indicate lower and upper bound in the type signature *)

(* if the same set of types have < and > indicator the exact type will result from it *)
let args = [ three; four ]
let exact = List.filter ~f:is_positive args

(* has a greater type *)
let is_positive' = function
  | `Int x -> Ok (x > 0)
  | `Float x -> Ok Float.(x > 0.)
  | `Not_a_number -> Error "Not a number"
;;

(* infers the lower bound type *)
let with_different_types =
  List.filter
    ~f:(fun x ->
      match is_positive' x with
      | Error _ -> false
      | Ok b -> b)
    args
;;

(* using catch-all cases with polymorphic variants leads to runtime errors as types will become > *)

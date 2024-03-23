(* Maps and Hash Tables *)

open Base

(* simple association list *)
let digital_alist =
  [ 0, "zero"
  ; 1, "one"
  ; 2, "two"
  ; 3, "three"
  ; 4, "four"
  ; 5, "fice"
  ; 6, "six"
  ; 7, "seven"
  ; 8, "eight"
  ; 9, "nine"
  ]
;;

(* there as assoc list helper in base *)
List.Assoc.find ~equal:Int.equal digital_alist 6;;
List.Assoc.find ~equal:Int.equal digital_alist 2;;
List.Assoc.add ~equal:Int.equal digital_alist 0 "zilch"

(* assoc list are not very efficient hence there are maps & hash tables
   maps - three-like with logarithmic time complexity
   hash-tables - mutable, with constant time time complexity *)

(* Maps *)
module type Counter = sig
  (** A collection of string frequency counts *)
  type t

  (** The empty set of frequency counts *)
  val empty : t

  (** Bump the frequency count for the given string *)
  val touch : t -> string -> t

  (** Converts the set of frequency counts to an association list.
      Every string int the list will show up the most once, and the integers
      will be at least 1. *)
  val to_list : t -> (string * int) list
end

module Map_counter : Counter = struct
  type t = (string, int, String.comparator_witness) Map.t

  let empty = Map.empty (module String)

  let touch t s =
    let count =
      match Map.find t s with
      | None -> 0
      | Some x -> x
    in
    Map.set t ~key:s ~data:(count + 1)
  ;;

  let to_list t = Map.to_alist t
end



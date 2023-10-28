open Base

type 'a element =
  { value : 'a
  ; mutable next : 'a element option
  ; mutable prev : 'a element option
  }

type 'a t = 'a element option ref

(** Basic list operations *)
let create () = ref None

let is_empty t = Option.is_none !t

(** Navigation using [element]s *)
let value elt = elt.value

let first t = !t
let next elt = elt.next
let prev elt = elt.prev

(** Mutation *)
let insert_first t value =
  let new_elt = { prev = None; next = !t; value } in
  (match !t with
   | Some old_first -> old_first.prev <- Some new_elt
   | None -> ());
  (* update the ref to point to new head *)
  t := Some new_elt;
  new_elt
;;

let insert_after elt value =
  let new_elt = { prev = Some elt; next = elt.next; value } in
  (match elt.next with
   | Some old_next -> old_next.prev <- Some new_elt
   | None -> ());
  elt.next <- Some new_elt;
  new_elt
;;

(*
   this code is very fragile as it would allow to remove
   an element is does not even belong to

   this case happens in case an element is removed twice
   from a list
*)
let remove t elt =
  let { prev; next; _ } = elt in
  (match prev with
   | Some prev -> prev.next <- next
   | None -> t := next);
  (match next with
   | Some next -> next.prev <- prev
   | None -> ());
  elt.prev <- None;
  elt.next <- None
;;

(** Whole-data-structure iteration *)
let iter t ~f =
  let rec loop = function
    | None -> ()
    | Some el ->
      f (value el);
      loop (next el)
  in
  loop !t
;;

let find_el t ~f =
  let rec loop = function
    | None -> None
    | Some elt -> if f (value elt) then Some elt else loop (next elt)
  in
  loop !t
;;

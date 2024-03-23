open! Core
open Core_bench

exception Exit

let x = 0

type how_to_end =
  | Ordinary
  | Raise
  | Raise_no_backtrace

let computation how_to_end =
  let x = 10 in
  let y = 40 in
  let _z = x + (y * y) in
  match how_to_end with
  | Ordinary -> ()
  | Raise -> raise Exit
  | Raise_no_backtrace -> raise_notrace Exit
;;

let computation_with_handler how =
  try computation how with
  | Exit -> ()
;;

let () =
  [ Bench.Test.create ~name:"simple computation" (fun () -> computation Ordinary)
  ; Bench.Test.create ~name:"computation w/handler" (fun () ->
      computation_with_handler Ordinary)
  ; Bench.Test.create ~name:"end with exn" (fun () -> computation_with_handler Raise)
  ; Bench.Test.create ~name:"end with exn notrace" (fun () ->
      computation_with_handler Raise_no_backtrace)
  ]
  |> Bench.make_command
  |> Command_unix.run
;;

(* Run benchmark with: dune exec -- errhandling/bench_backtraces.exe -ascii -quota 1s*)

(*
   Name                    Time/Run   Percentage
   ----------------------- ---------- ------------
   simple computation        2.03ns       14.28%
   computation w/handler     2.18ns       15.36%
   end with exn             14.23ns      100.00%
   end with exn notrace      8.71ns       61.20%
*)

(* with traces is about 2x slower than with out *)

(* using OCAMLRUNPARAM=b=0 here only as a very light effect *)

(*
   Name                    Time/Run   Percentage
   ----------------------- ---------- ------------
   simple computation        2.03ns       23.33%
   computation w/handler     2.18ns       24.99%
   end with exn              8.71ns      100.00%
   end with exn notrace      8.70ns       99.94%
*)

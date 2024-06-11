(** Implement analog to the unix md5 util.

    Like v2 but uses let syntax. *)
open Core

let do_hash hash_length file =
  Md5.digest_file_blocking file
  |> Md5.to_hex
  |> fun s -> String.prefix s hash_length |> print_endline
;;

let hash_length =
  let open Command.Param in
  anon ("hash_length" %: int)
;;

let filename_param =
  let open Command.Param in
  anon ("filename" %: string)
;;

let command =
  Command.basic
    ~summary:"Generate an MD5 hash of the input data"
    ~readme:(fun () -> "More detailed information")
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map hash_length = anon ("hash_length" %: int)
     and filename = anon ("filename" %: string) in
     fun () -> do_hash hash_length filename)
;;

let () = Command_unix.run ~version:"3.0" ~build_info:"RWO" command

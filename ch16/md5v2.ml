(** Implement analog to the unix md5 util.

    Implementation with two anonymous parameters combines using [both]
    to combine the parsers. *)
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
    Command.Param.(
      map (both hash_length filename_param) ~f:(fun (hash_length, filename) () ->
        do_hash hash_length filename))
;;

let () = Command_unix.run ~version:"2.0" ~build_info:"RWO" command

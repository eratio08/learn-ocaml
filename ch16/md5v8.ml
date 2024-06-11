(** Implement analog to the unix md5 util.

    Using a optional argument with default. *)
open Core

let get_content = function
  | "-" -> In_channel.input_all In_channel.stdin
  | filename -> In_channel.read_all filename
;;

let do_hash filename =
  get_content filename |> Md5.digest_string |> Md5.to_hex |> print_endline
;;

let command =
  Command.basic
    ~summary:"Generate an MD5 hash of the input data"
    ~readme:(fun () -> "More detailed information")
    (let%map_open.Command file =
       anon (maybe_with_default "-" ("filename" %: Filename_unix.arg_type))
     in
     fun () -> do_hash file)
;;

let () = Command_unix.run ~version:"8.0" ~build_info:"RWO" command

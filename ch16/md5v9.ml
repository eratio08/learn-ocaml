(** Implement analog to the unix md5 util.

    Using variable length arguments. *)
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
    (let%map_open.Command files =
       anon (sequence ("filename" %: Filename_unix.arg_type))
     in
     fun () ->
       match files with
       | [] -> do_hash "-"
       | _ -> List.iter files ~f:do_hash)
;;

let () = Command_unix.run ~version:"9.0" ~build_info:"RWO" command

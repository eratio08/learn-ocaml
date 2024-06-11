(** Implement analog to the unix md5 util.

    Like v4 but uses using predefined argument type, here [Filename_unix.arg_type]. *)
open Core

let do_hash file = Md5.digest_file_blocking file |> Md5.to_hex |> print_endline

let command =
  Command.basic
    ~summary:"Generate an MD5 hash of the input data"
    ~readme:(fun () -> "More detailed information")
    (let%map_open.Command file = anon ("filename" %: Filename_unix.arg_type) in
     fun () -> do_hash file)
;;

let () = Command_unix.run ~version:"5.0" ~build_info:"RWO" command

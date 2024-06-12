(** Implement analog to the unix md5 util.

    Using a custom argument type. *)
open Core

let do_hash file = Md5.digest_file_blocking file |> Md5.to_hex |> print_endline

let regular_file =
  Command.Arg_type.create (fun filename ->
    match Sys_unix.is_file filename with
    | `Yes -> filename
    | `No -> failwith "Not a regular file"
    | `Unknown -> failwith "Could not determine if this was a regular file")
;;

let command =
  Command.basic
    ~summary:"Generate an MD5 hash of the input data"
    ~readme:(fun () -> "More detailed information")
    (let%map_open.Command file = anon ("filename" %: regular_file) in
     fun () -> do_hash file)
;;

let () = Command_unix.run ~version:"6.0" ~build_info:"RWO" command

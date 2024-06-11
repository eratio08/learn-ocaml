(*** Calendar helper, using dedicated parser for interactive input. *)

open Core

let add_days base days = Date.add_days base days |> Date.to_string |> print_endline

let prompt_for_string name of_string =
  printf "enter %s: %!" name;
  match In_channel.input_line In_channel.stdin with
  | None -> failwith "no value entered, aboring"
  | Some line -> of_string line
;;

let anon_promt name of_string =
  let arg = Command.Arg_type.create of_string in
  let%map_open.Command value = anon (maybe (name %: arg)) in
  match value with
  | Some v -> v
  | None -> prompt_for_string name of_string
;;

let add =
  Command.basic
    ~summary:"Add [days] to the [base] date and print day"
    (let%map_open.Command base = anon ("base" %: date)
     and days = anon_promt "days" Int.of_string in
     fun () -> Date.add_days base days |> Date.to_string |> print_endline)
;;

let () = Command_unix.run add

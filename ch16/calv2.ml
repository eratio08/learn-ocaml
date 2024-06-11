(*** Calendar helper, using interactive input. *)

open Core

let add_days base days = Date.add_days base days |> Date.to_string |> print_endline

let prompt_for_string name of_string =
  printf "enter %s: %!" name;
  match In_channel.input_line In_channel.stdin with
  | None -> failwith "no value entered, aboring"
  | Some line -> of_string line
;;

let add =
  Command.basic
    ~summary:"Add [days] to the [base] date and print day"
    (let%map_open.Command base = anon ("base" %: date)
     and days = anon (maybe ("days" %: int)) in
     let days =
       match days with
       | Some x -> x
       | None -> prompt_for_string "days" Int.of_string
     in
     fun () -> Date.add_days base days |> Date.to_string |> print_endline)
;;

let () = Command_unix.run add

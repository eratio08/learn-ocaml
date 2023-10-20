(* IO *)

open Core

let () =
  Out_channel.output_string stdout "Pick a timezone: ";
  Out_channel.flush stdout;
  match In_channel.(input_line stdin) with
  | None -> failwith "No timezone provided"
  | Some zone_string ->
    let zone = Time_float_unix.Zone.find_exn zone_string in
    let time_string = Time_float_unix.to_string_abs (Time_float_unix.now ()) ~zone in
    Out_channel.output_string
      stdout
      (String.concat
         [ "The time in "
         ; Time_float_unix.Zone.to_string zone
         ; " is "
         ; time_string
         ; ".\n"
         ]);
    Out_channel.flush stdout
;;

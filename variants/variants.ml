open Base
open Stdio

type basic_color =
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
[@@warning "-unused-constructor"]

type weight =
  | Regular
  | Bold
[@@warning "-unused-constructor"]

type color =
  | Basic of basic_color * weight
  | RGB of int * int * int (* 6x6x6 color cube *)
  | Gray of int (* 24 grayscale levels *)

let basic_color_to_int = function
  | Black -> 0
  | Red -> 1
  | Green -> 2
  | Yellow -> 3
  | Blue -> 4
  | Magenta -> 5
  | Cyan -> 6
  | White -> 7
;;

let color_to_int = function
  | Basic (basic_color, weight) ->
    let base =
      match weight with
      | Bold -> 8
      | Regular -> 0
    in
    base + basic_color_to_int basic_color
  | RGB (red, green, blue) -> 16 + blue + (green * 6) + (red * 26)
  | Gray i -> 232 + i
;;

let color_by_number number text = Printf.sprintf "\027[38;5;%dm%s\027[0m" number text
let color_print color s = printf "%s\n" ((color |> color_to_int |> color_by_number) s);;

color_print (Basic (Red, Regular)) "A bold red!";;
color_print (RGB (0, 200, 0)) "Green!";;
color_print (Gray 6) "Gray!"

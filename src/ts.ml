(** This module provides functions to manipulate timeseries data *)


open Exceptions
module Log = Dolog.Log


(** Warp 10 line *)
type line = Timed of int * string * string | Untimed of string * string

(** Warp 10 stream *)
type t = line list

(** Convert a string to timeseries data *)
let of_string str =
  let parse_time str = match String.split_on_char '/' str with
    | t::""::""::[] -> int_of_string t
    | _ -> raise Invalid_timeseries in
  let line_of_string str = match String.split_on_char ' ' str with
    | timestamp::series::value::[] -> Timed (parse_time timestamp, series, value)
    | prefix::value::[] -> begin
        try Timed (parse_time prefix, "{}", value) with
        | Invalid_timeseries -> Untimed (prefix, value)
      end
    | value::[] -> Untimed ("{}", value)
    | _ -> raise Invalid_timeseries in
  let rec of_string_rec acc = function
    | ""::[] -> (List.rev acc, None)
    | str::[] -> Log.debug "Received truncated input"; (List.rev acc, Some str)
    | str::tl -> of_string_rec ((String.trim str |> line_of_string)::acc) tl
    | [] -> raise Invalid_timeseries in
  String.split_on_char '\n' str |> of_string_rec []

(** Format a timeseries with prefixed components *)
let format components now =
  let formatted_components = String.concat "." components in
  let now_us = now *. 1000000. |> int_of_float in
  let format_series series =
    let series = match series.[0] with
      | '{' -> formatted_components ^ series
      | _ -> begin
          match formatted_components with
          | "" -> series
          | _ -> formatted_components ^ "." ^ series
        end in
    match series.[String.length series - 1] with
      | '}' -> series
      | _ -> series ^ "{}" in
  let format_rec = function
    | Timed (ts, series, value) -> Timed (ts, format_series series, value)
    | Untimed (series, value) -> Timed (now_us, format_series series, value) in
  List.map format_rec

(** Convert a timeseries line to its string representation *)
let string_of_line = function
  | Timed (ts, series, value) -> string_of_int ts ^ "// " ^ series ^ " " ^ value
  | Untimed (series, value) -> "// " ^ series ^ " " ^ value

(** Convert timeseries data to string *)
let to_string = List.fold_left (fun acc line -> acc ^ string_of_line line ^ "\n") ""

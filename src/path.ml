(** This module provides helper functions to manipulate file paths *)


(** Convert a path to its components *)
let to_components path = match String.split_on_char '/' path with
  | ""::""::[] -> []
  | ""::tl -> tl
  | _ -> raise (Invalid_argument "path")

(** Get the basename of a path *)
let basename_opt path = match to_components path with
  | [] -> None
  | l -> List.rev l |> List.hd |> Option.some

(** Get the dirname components of a path *)
let dirname_components path = match to_components path with
  | [] -> []
  | l -> List.rev l |> List.tl |> List.rev

(** Remove the first character of a name *)
let sub1 str = String.sub str 1 (String.length str - 1)

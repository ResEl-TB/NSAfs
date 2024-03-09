(** This module provides functions to manipulate policies *)


open Exceptions
open Types


(** Convert a string to a policy *)
let of_string str =
  try
    match Lexing.from_string str |> Policy_parser.start Policy_lexer.token with
    | Bounded (At_least n, _, At_least n') when n > n' -> raise Illegal_policy
    | p -> p
  with Policy_parser.Error | Failure _ -> raise Illegal_policy

(** Convert a number of seconds to a time string *)
let string_of_time t =
  let string_of_seconds n prefix suffix t = match t/n with
    | 0 -> (prefix, t)
    | i -> (prefix ^ string_of_int i ^ suffix, t mod n) in
  let (str, t) = string_of_seconds 604800 "" "w" t in
  let (str, t) = string_of_seconds 86400 str "d" t in
  let (str, t) = string_of_seconds 3600 str "h" t in
  let (str, t) = string_of_seconds 60 str "m" t in
  match t with
  | 0 -> str
  | n -> str ^ string_of_int n ^ "s"

(** Convert a rule to a string *)
let string_of_rule = function
  | At_least 0 -> "now"
  | At_least n -> string_of_int n
  | Every (t, 0) -> string_of_time t
  | Every (t, t') -> string_of_time t ^ "+" ^ string_of_time t'

(** Convert a policy to its string representation *)
let to_string = function
  | Only rule -> string_of_rule rule
  | First_of (rule, rule') -> string_of_rule rule ^ "_" ^ string_of_rule rule'
  | Last_of (rule, rule') -> string_of_rule rule ^ "^" ^ string_of_rule rule'
  | Bounded (rule, rule', rule'') ->
      string_of_rule rule ^ "^" ^ string_of_rule rule' ^ "_" ^ string_of_rule rule''

(** Normalize a policy *)
let normalize str = of_string str |> to_string

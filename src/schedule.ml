(** This module provides functions to handle schedules *)


open Exceptions
open Types


(** Get the current (float) UNIX time *)
let now = Unix.gettimeofday

(** Get the next (int) UNIX time given a period and an offset *)
let next t t' = (int_of_float (now ()) / t + 1) * t + t'

(** Create a schedule from a policy *)
let of_policy = function
  | Only (At_least n) -> Require n
  | Only (Every (t, t')) -> At (next t t')
  | First_of (At_least n, Every (t, t')) -> Any (n, next t t')
  | Last_of (At_least n, Every (t, t')) -> All (n, next t t')
  | Bounded (At_least n, Every (t, t'), At_least n') -> Threshold (n, Future (t, t'), n')
  | _ -> raise Illegal_policy

(** Convert a schedule to a string *)
let to_string = function
  | Require n -> "#" ^ string_of_int n
  | At t -> "@" ^ string_of_int t
  | Any (n, t) -> "#" ^ string_of_int n ^ "_@" ^ string_of_int t
  | All (n, t) -> "#" ^ string_of_int n ^ "^@" ^ string_of_int t
  | Threshold (n, f, n') ->
      "#" ^ string_of_int n ^ "^" ^ Future.to_string f ^ "_@" ^ string_of_int n'

(** Total order over schedules *)
let compare s s' = match (s, s') with
  | At t, At t' -> compare t t'
  | At t, Any (_, t') -> if t = t' then -1 else compare t t'
  | At _, _ -> -1
  | Require i, Require j -> compare i j
  | Require _, Any _ -> 1
  | Require i, All (j, _) -> if i = j then -1 else compare i j
  | Any (_, t), At t' -> if t = t' then 1 else compare t t'
  | Any _, Require _ -> -1
  | Any (n, t), Any (n', t') when t = t' -> compare n n'
  | Any (_, t), Any (_, t') -> compare t t'
  | Any _, All _ -> -1
  | All (i, _), Require j -> if i = j then 1 else compare i j
  | All _, Any _ -> 1
  | All (n, t), All (n', t') when t = t' -> compare n n'
  | All (_, t), All (_, t') -> compare t t'
  | Threshold (n, f, n'), Threshold (n'', f', n''') when f = f' && n = n'' -> compare n' n'''
  | Threshold (n, f, _), Threshold (n', f', _) when f = f' -> compare n n'
  | Threshold (_, Planned _, _), Threshold (_, Future _, _) -> -1
  | Threshold (_, Future _, _), Threshold (_, Planned _, _) -> 1
  | Threshold (_, Planned t, _), Threshold (_, Planned t', _) -> compare t t'
  | Threshold (_, Future (t, t'), _), Threshold (_, Future (t'', t'''), _) when t = t'' ->
      compare t' t'''
  | Threshold (_, Future (t, _), _), Threshold (_, Future (t', _), _) -> compare t t'
  | _, At _ -> 1
  | _, Threshold _ -> -1
  | Threshold _, _ -> 1

(** Get the minimum of two schedules *)
let min s s' = if compare s s' <= 0 then s else s'

(** Get Some up-to-date schedule or None if the schedule is due *)
let update s n t = match (s, n, t) with
  | At t, _, t'
  | Threshold (_, Planned t, _), _, t' when t <= t' -> None
  | Require n, n', _
  | Threshold (_, _, n), n', _ when n <= n' -> None
  | Any (n, t), n', t' when n <= n' || t <= t' -> None
  | All (n, t), n', t' when n <= n' && t <= t' -> None
  | Threshold (n, Future (t, t'), n'), n'', _ when n <= n'' ->
      Some (Threshold (n, Planned (next t t'), n'))
  | _ -> Some s

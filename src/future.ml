(** This module provides functions to handle future times *)


open Types


(** Comparison of future times *)
let min fut fut' = match (fut, fut') with
  | Planned t, Planned t' -> Planned (min t t')
  | Planned _, Future _ -> fut
  | Future _, Planned _ -> fut'
  | Future (t, _), Future (t', _) when t < t' -> fut
  | Future (t, t'), Future (t'', t''') when t = t'' -> Future (t, min t' t''')
  | _ -> fut'

(** Convert a future time to a string *)
let to_string = function
  | Planned t -> "@" ^ string_of_int t
  | Future (t, t') -> "?" ^ string_of_int t ^ "," ^ string_of_int t'

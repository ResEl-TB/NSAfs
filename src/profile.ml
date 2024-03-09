(** This module provides functions to manipulate module profiles *)


open Types


(** Get a profile from a list of components *)
let of_components components =
  let rec of_components_rec profile = function
    | name'::tl, ({ name; _ } as profile::_) when name = name' -> of_components_rec profile (tl, profile.sub)
    | components, _::tl -> of_components_rec profile (components, tl)
    | [], _ -> profile
    | _, _ -> { profile with sub = [] } in
  of_components_rec !Constants.profiles (components, !Constants.profiles.sub)

(** Get a profile from a path *)
let of_path path = Path.to_components path |> of_components

(** Convert a profile to a string *)
let to_string { name; policy; _ } = "|" ^ name ^ ":" ^ Policy.to_string policy ^ "|"

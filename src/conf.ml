(** Configuration file handler *)


open Exceptions


(** The configuration data *)
let data = ref None

(** Init the configuration from a file *)
let init file = data := Toml.Parser.(from_filename file |> unsafe) |> Option.some

(** Parse a TOML integer *)
let int = function
  | Toml.Types.TInt i -> i
  | _ -> raise (Type_error "an integer")

(** Parse a TOML string *)
let string = function
  | Toml.Types.TString s -> s
  | _ -> raise (Type_error "a string")

(** Get a TOML field recursively *)
let get ty =
  let rec get_rec data = function
    | key::tl -> begin
        try
          match (tl, Toml.(Types.Table.find (Min.key key)) data) with
          | _::_, Toml.Types.TTable data' -> get_rec data' tl
          | [], res -> ty res
          | _, _ -> raise (Type_error "a nested table")
        with
        | Type_error t -> raise (Configuration_error [%string "`%{key}' must be %{t}"])
        | Not_found -> raise (Configuration_error [%string "`%{key}' parameter required but not provided"])
      end
    | [] -> raise (Invalid_argument "get") in
  Option.get !data |> get_rec

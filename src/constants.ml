(** NSAfs constants *)


open Types
module Log = Dolog.Log


(** Mutex to protect mutable values *)
let mutex = Mutex.create ()

(** Default module profile *)
let default_profile = { name = ""; token = ""; policy = Policy.of_string "1m"; evict = true;
                        sub = [] }

(** NSAfs owner *)
let uid = ref 0
let gid = ref 0

(** Block size *)
let f_bsize = 4096L

(** Maximum allocated memory *)
let max_memory = ref 0L

(** Configuration profiles *)
let profiles = ref default_profile

(** Eviction policy *)
let evict_to = ref None

(** Error directory *)
let error_to = ref ""

(** Warp 10 jobs *)
let send_jobs = ref 1

(** Warp 10 endpoint *)
let endpoint = ref ""

(** Parse a memory string to an Int64 *)
let parse_memory str =
  let length = String.length str in
  let value = String.sub str 0 (length - 1) |> Int64.of_string in
  Int64.mul value begin
    match String.sub str (length - 1) 1 with
      | "k" | "K" -> 1024L
      | "m" | "M" -> 1048576L
      | "g" | "G" -> 1073741824L
      | _ -> raise (Invalid_argument "parse_memory")
    end

(** Parse the configuration profiles *)
let parse_profiles () =
  let rec parse_profile data = Toml.Types.Table.fold parse_line data
  and parse_line key value ({ token; policy; evict; sub; _ } as inherited) =
    match (Toml.Types.Table.Key.to_string key, value) with
    | "token", value -> { inherited with token = Conf.string value }
    | "policy", value -> { inherited with policy = Conf.string value |> Policy.of_string }
    | name, Toml.Types.TTable data when name <> "NSAfs" ->
        let name = if String.sub name 0 2 = "__"
        then String.sub name 2 (String.length name - 2)
        else name in
        { inherited with sub = parse_profile data { name; token; policy; evict; sub = [] }::sub }
    | _ -> inherited in
  parse_profile (Option.get !Conf.data) default_profile

(** Reload the configuration *)
let reload () =
  Mutex.lock mutex;
  Option.value ~default:"/etc/nsa.conf" (Sys.getenv_opt "NSA_CONF") |> Conf.init;
  let { Unix.pw_uid; _ } = Unix.getpwnam "nsa" in
  let { Unix.gr_gid; _ } = Unix.getgrnam "nsa" in
  uid := pw_uid;
  gid := gr_gid;
  begin
    match Conf.(get string) ["NSAfs"; "log_to"] with
    | "<stdout>" -> stdout
    | "<stderr>" -> stderr
    | f -> open_out_gen [Open_wronly; Open_append; Open_creat; Open_text] 0o640 f
  end |> Log.set_output;
  Conf.(get string) ["NSAfs"; "log_level"] |> Log.level_of_string |> Log.set_log_level;
  max_memory := Conf.(get string) ["NSAfs"; "max_memory"] |> parse_memory;
  evict_to := begin
    match Conf.(get string) ["NSAfs"; "evict_to"] with
    | "<nowhere>" -> None
    | path -> Some path
  end;
  profiles := parse_profiles ();
  send_jobs := Conf.(get int) ["NSAfs"; "send_jobs"];
  endpoint := Conf.(get string) ["NSAfs"; "endpoint"];
  error_to := Conf.(get string) ["NSAfs"; "error_to"];
  Mutex.unlock mutex;
  Log.debug "Loaded configuration"

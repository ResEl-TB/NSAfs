(** This module provides a shared spool of data as the internal storage for NSAfs *)


open Types
module Log = Dolog.Log


(** Mutex to protect the spool *)
let spool_mutex = Mutex.create ()

(** Mutex to protect the continued data *)
let cont_mutex = Mutex.create ()

(** Internal spool mapping profiles to scheduled timeseries *)
let (spool: (profile, (schedule * Ts.t) list) Hashtbl.t) = Hashtbl.create ~random:true 16

(** Continued data table mapping file names to the excess data *)
let (cont: (string, string) Hashtbl.t) = Hashtbl.create ~random:true 16

(** Convert a spool to its string representation *)
let to_string () =
  let string_of_job (schedule, ts) =
    "    " ^ Schedule.to_string schedule ^ " => \n"
           ^ (List.fold_left (fun acc line -> acc ^ "      " ^ Ts.string_of_line line ^ "\n")
                             "" ts) in
  let string_of_entry profile jobs =
    "  " ^ Profile.to_string profile ^ " => \n"
         ^ (List.fold_left (fun acc job -> acc ^ string_of_job job) "" jobs) in
  "{\n" ^ Hashtbl.fold (fun profile jobs acc -> string_of_entry profile jobs ^ acc) spool "" ^ "}"

(** Perform an in-place filtering of data in the spool *)
let inplace_filter f =
  let filter profile jobs = match f profile jobs with
  | [] -> None
  | l -> Some l in
  Hashtbl.filter_map_inplace filter spool

(** Evict the oldest evictable data from the spool *)
let evict () =
  let save_evicted = match !Constants.evict_to with
    | Some dir ->
        let file = dir ^ "/oom." ^ string_of_int (int_of_float (Schedule.now ())) in
        begin fun data ->
          let out = open_out_gen [Open_wronly; Open_append; Open_creat; Open_text] 0o640 file in
          output_string out data;
          close_out out
        end
    | None -> fun _ -> () in
  let jobs_min = function
    | (s, _)::tl -> List.fold_left (fun min (s, _) -> Schedule.min min s) s tl
    | [] -> raise (Invalid_argument "evict") in
  let evicted_s = Hashtbl.to_seq spool
                  |> Seq.filter_map (fun ({ evict ;_ }, jobs) -> if evict then Some jobs else None)
                  |> List.of_seq |> List.flatten |> jobs_min in
  let filter_evicted (s, lines) = match s = evicted_s with
    | true -> Ts.to_string lines |> save_evicted; false
    | false -> true in
  Log.warn "Evicting %s" (Schedule.to_string evicted_s);
  inplace_filter (fun _ -> List.filter filter_evicted)

(** Extract the timeseries whose schedule is due *)
let extract_current () =
  let now = Schedule.now () |> int_of_float in
  let current = Hashtbl.create 8 in
  let filter { token; _ } =
    let rec filter_rec acc = function
      | (schedule, ts)::tl -> begin
          match Schedule.update schedule (List.length ts) now with
          | None ->
              begin
                match Hashtbl.find_opt current token with
                | Some l -> l @ ts
                | None -> ts
              end |> Hashtbl.replace current token;
              filter_rec acc tl
          | Some schedule -> filter_rec ((schedule, ts)::acc) tl
        end
      | [] -> List.rev acc in
    filter_rec [] in
  Mutex.lock spool_mutex;
  inplace_filter filter;
  Mutex.unlock spool_mutex;
  current

(** Write data to the spool *)
let write token schedule data =
  let rec write_rec acc jobs = match (jobs, schedule) with
    | (At t, data')::tl, At t' when t = t' -> List.rev_append ((At t, data' @ data)::acc) tl
    | (Require n, data')::tl, Require n' when n = n' ->
        List.rev_append ((Require n, data' @ data)::acc) tl
    | (Any (n, t), data')::tl, Any (n', t') when n = n' ->
        List.rev_append ((Any (n, min t t'), data' @ data)::acc) tl
    | (All (n, t), data')::tl, All (n', t') when n = n' ->
        List.rev_append ((All (n, min t t'), data' @ data)::acc) tl
    | (Threshold (n, fut, n'), data')::tl, Threshold (n'', fut', n''') when n = n'' && n' = n''' ->
        List.rev_append ((Threshold (n, Future.min fut fut', n'), data' @ data)::acc) tl
    | hd::tl, _ -> write_rec (hd::acc) tl
    | [], _ -> List.rev ((schedule, data)::acc) in
  Mutex.lock spool_mutex;
  while Memory.is_full () do
    evict ()
  done;
  Hashtbl.find_opt spool token |> Option.to_list |> List.flatten |> write_rec []
                               |> Hashtbl.replace spool token;
  Mutex.unlock spool_mutex

(** Push data to the continued data table *)
let push path data =
  Mutex.lock cont_mutex;
  Option.iter (Hashtbl.replace cont path) data;
  Mutex.unlock cont_mutex

(** Pop data from the continued data table *)
let pop path =
  Mutex.lock cont_mutex;
  let data = Hashtbl.find_opt cont path in
  Hashtbl.remove cont path;
  Mutex.unlock cont_mutex;
  data

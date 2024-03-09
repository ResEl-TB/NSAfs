(** This module handles the parallel transmission of data to Warp 10 *)


module Log = Dolog.Log


(** Thread main loop *)
let rec process () =
  let semaphore = Semaphore.Counting.make !Constants.send_jobs in
  let endpoint = !Constants.endpoint in
  let file = !Constants.error_to ^ "/err." ^ string_of_int (int_of_float (Schedule.now ())) in
  let send token ts =
    Semaphore.Counting.acquire semaphore;
    let data = Ts.to_string ts |> Ezgzip.compress in
    let headers = [("X-Warp10-Token", token); ("Content-Type", "application/gzip")] in
    begin match Ezcurl.post ~headers ~content:(`String data) ~url:endpoint ~params:[] () with
    | Ok { code = 200; _ } -> ()
    | Ok { body = e; _ } | Error (_, e) ->
        Log.error "%s" e; 
        let out = open_out_gen [Open_wronly; Open_append; Open_creat; Open_text] 0o640
                               (file ^ "." ^ string_of_int (Random.int 1000000000)) in
        output_string out data;
        close_out out
    end;
    Semaphore.Counting.release semaphore in
  Hashtbl.fold (fun token ts threads -> Thread.create (send token) ts::threads)
               (Spool.extract_current ()) [] |> List.iter Thread.join;
  Unix.sleepf 0.3;
  process ()

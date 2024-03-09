(** This module provides low-level memory functions *)


module Log = Dolog.Log


(** Get memory statistics *)
let stat = Gc.quick_stat

(** Get approximate memory usage in bytes *)
let usage () =
  let { Gc.heap_words; _ } = stat () in
  heap_words * Sys.word_size / 8 |> Int64.of_int

(** Get approximate free memory in bytes *)
let free () = usage () |> Int64.sub !Constants.max_memory

(** Perform a full garbage collection *)
let gc = Gc.full_major

(** Check if the memory is full *)
let is_full () =
  let check_full () = Int64.div !Constants.max_memory 10L |> Int64.mul 9L < usage () in 
  if check_full ()
  then (gc (); check_full ())
  else false

(** Debugging alert *)
let _ = Gc.create_alarm (fun () -> Log.debug "Finished garbage collection")

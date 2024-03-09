(** This module helps to manipulate byte buffers *)


module M = Bigarray.Array1


(** Get the size of the buffer *)
let length = M.dim

(** Convert a buffer to a string *)
let to_string buf = String.init (length buf) (fun i -> M.unsafe_get buf i)

(** Convert a string to a buffer *)
let of_string str = 
  let buf = String.length str |> M.create Bigarray.char Bigarray.c_layout in
  String.iteri (fun i chr -> M.unsafe_set buf i chr) str;
  buf

(** Copy the elements of a buffer to another *)
let blit = M.blit

(** Extract a subarray *)
let sub = M.sub

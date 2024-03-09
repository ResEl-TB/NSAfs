(** This module provides a string representation for Unix open tags *)


type t = Unix.open_flag


(** Convert a tag to a string *)
let to_string = function
  | Unix.O_RDONLY -> "O_RDONLY"
  | O_WRONLY -> "O_WRONLY"
  | O_RDWR -> "O_RDWR"
  | O_NONBLOCK -> "O_NONBLOCK"
  | O_APPEND -> "O_APPEND"
  | O_CREAT -> "O_CREAT"
  | O_TRUNC -> "O_TRUNC"
  | O_EXCL -> "O_EXCL"
  | O_NOCTTY -> "O_NOCTTY"
  | O_DSYNC -> "O_DSYNC"
  | O_SYNC -> "O_SYNC"
  | O_RSYNC -> "O_RSYNC"
  | O_SHARE_DELETE -> "O_SHARE_DELETE"
  | O_CLOEXEC -> "O_CLOEXEC"
  | O_KEEPEXEC -> "O_KEEPEXEC"

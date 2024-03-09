(** This module provides the code for FUSE to handle our filesystem *)


open Exceptions
open Fuse
open Types
module Log = Dolog.Log


(** readdir operation *)
let readdir path _ =
  Log.debug "readdir %s" path;
  let { sub; _ } = Profile.of_path path in
  let dirs = "."::".."::"@"::"@now"::List.map (fun { name; _ } -> name) sub in
  match path with
  | "/" -> "??gc"::"??inspect"::"??memstats"::"??reload"::dirs
  | _ -> dirs

(** statfs operation *)
let statfs _ =
  Log.debug "statfs";
  let f_bsize = Constants.f_bsize in
  let f_blocks = Int64.div !Constants.max_memory f_bsize in
  let f_bfree = Int64.div (Memory.free ()) f_bsize in
  { Fuse.Unix_util.f_bsize; f_blocks; f_bfree; f_bavail = f_bfree; f_files = f_blocks;
    f_ffree = f_bfree; f_namemax = 1024L; f_frsize = 0L; f_favail = 0L; f_fsid = 0L; f_flag = 0L }

(** getattr operation *)
let getattr path =
  Log.debug "getattr %s" path;
  let (st_kind, st_perm, st_size) = match Path.basename_opt path with
  | Some "@" -> (Unix.S_LNK, 0o220, 0L)
  | Some ("@now" | "??gc" | "??reload") -> (Unix.S_REG, 0o220, 0L)
  | Some "??inspect" ->
      (Unix.S_REG, 0o440, Spool.to_string () ^ "\n" |> String.length |> Int64.of_int)
  | Some "??memstats" ->
      (Unix.S_REG, 0o440, 512L)
  | Some name when name.[0] = '@' ->
      let policy = Path.sub1 name in begin
        try
          let policy' = Policy.normalize policy in
          if policy = policy'
          then (S_REG, 0o220, 0L)
          else (S_LNK, 0o220, 0L)
        with
        | Illegal_policy -> raise (Unix.Unix_error (ENOENT, "getattr", path))
      end
  | _ -> (S_DIR, 0o770, 0L) in
  let now = Schedule.now () in
  { Unix.LargeFile.st_dev = 0; st_ino = 0; st_kind; st_perm; st_nlink = 1; st_uid = !Constants.uid;
    st_gid = !Constants.gid; st_rdev = 0; st_size; st_atime = now; st_mtime = now; st_ctime = now }

(** readlink operation *)
let readlink path =
  Log.debug "readlink %s" path;
  match Path.basename_opt path with
  | Some "@" -> "@" ^ ((Profile.of_path path).policy |> Policy.to_string)
  | Some name when name.[0] = '@' -> "@" ^ (Path.sub1 name |> Policy.normalize)
  | _ -> raise (Unix.Unix_error (ENOENT, "readlink", path))

(** Raise an EPERM error *)
let raise_eperm name = fun path -> raise (Unix.Unix_error (EPERM, name, path))

(** open operation *)
let fopen path flags =
  List.map Flag.to_string flags |> String.concat "|" |> Log.debug "fopen %s [%s]" path;
  match path with
  | "/??gc" | "/??reload" ->
      if List.mem Unix.O_WRONLY flags
      then None
      else raise_eperm "open" path
  | "/??inspect" | "/??memstats" ->
      if List.mem Unix.O_RDONLY flags
      then None
      else raise_eperm "open" path
  | _ ->
      if List.mem Unix.O_APPEND flags && List.mem Unix.O_WRONLY flags
      then None
      else raise_eperm "open" path

(** opendir operation *)
let opendir path flags =
  List.map Flag.to_string flags |> String.concat "|" |> Log.debug "opendir %s [%s]" path;
  if List.mem Unix.O_RDONLY flags
  then None
  else raise_eperm "opendir" path

(** write operation *)
let write path buf _ _ =
  Log.debug "write %s" path;
  let data = Buffer.to_string buf in
  try
    begin match path with
      | "/??gc" -> Memory.gc ()
      | "/??reload" -> Constants.reload ()
      | _ ->
          let { policy; _ } as profile = Profile.of_path path in
          let schedule = match Path.basename_opt path with
            | Some name when name.[0] = '@' ->
                Path.sub1 name |> Policy.of_string |> Schedule.of_policy
            | _ -> Schedule.of_policy policy in
          let (ts, cont) = Option.value ~default:"" (Spool.pop path) ^ data |> Ts.of_string in
          Spool.push path cont;
          Ts.format (Path.dirname_components path) (Schedule.now ()) ts
          |> Spool.write profile schedule
    end;
    let length = Buffer.length buf in
    Log.debug "<Written %i bytes to %s" length path;
    length
  with
  | Invalid_timeseries ->
      Log.error "Received invalid timeseries data: %s" data;
      raise (Unix.Unix_error (Unix.EINVAL, "write", path))

(** read operation *)
let read path buf offset _ =
  Log.debug "read %s" path;
  match path with
  | "/??inspect" ->
      if offset > Int64.of_int max_int
      then 0
      else
        let spool = Spool.to_string () ^ "\n" in
        let off = Int64.to_int offset in
        let len = min (String.length spool - off) (Buffer.length buf) in
        Buffer.blit (String.sub spool off len |> Buffer.of_string) (Buffer.sub buf 0 len);
        len
  | "/??memstats" ->
      if offset > Int64.of_int max_int
      then 0
      else
        let { Gc.minor_words; promoted_words; major_words; minor_collections;
              major_collections; heap_words; compactions; top_heap_words;
              forced_major_collections; _ } = Memory.stat () in
        let data = [%string "words.total{type=minor} %{minor_words # Float}\n\
                             words.total{type=promoted} %{promoted_words # Float}\n\
                             words.total{type=major} %{major_words # Float}\n\
                             collections.total{type=minor} %{minor_collections # Int}\n\
                             collections.total{type=major} %{major_collections # Int}\n\
                             words{type=major} %{heap_words # Int}\n\
                             compactions.total{} %{compactions # Int}\n\
                             words.max{type=major} %{top_heap_words # Int}\n\
                             collections.forced.total{type=major} \
                               %{forced_major_collections # Int}\n"] in
        let off = Int64.to_int offset in
        let len = min (String.length data) (Buffer.length buf) in
        Buffer.blit (String.sub data off len |> Buffer.of_string) (Buffer.sub buf 0 len);
        len
  | _ -> raise_eperm "read" path

(** Arity 1 no-op *)
let nop _ = ()

(** Arity 2 no-op *)
let nop2 _ _ = ()

(** Arity 3 no-op *)
let nop3 _ _ _ = ()

(** getxattr operation *)
let getxattr path = raise (Unix.Unix_error (Unix.EUNKNOWNERR 61, "getxattr", path)) (* ENODATA *)

(** listxattr operation *)
let listxattr _ = []

(** Run a thread forever *)
let forever f = fun () -> while true do Thread.create f () |> Thread.join done

(** Main loop *)
let _ =
  Constants.reload ();
  Log.info "Started NSAfs";
  Log.debug "uid=%i, gid=%i" !Constants.uid !Constants.gid;
  let _ = Thread.create (forever Warp10.process) () in
  main Sys.argv {
    getattr; readlink; fopen; write; statfs; getxattr; listxattr; opendir; readdir; read;
    init = nop; releasedir = nop3; fsyncdir = nop3; flush = nop2; release = nop3; fsync = nop3;
    mknod = raise_eperm "mknod";
    mkdir = raise_eperm "mkdir";
    unlink = raise_eperm "unlink";
    rmdir = raise_eperm "rmdir";
    symlink = raise_eperm "symlink";
    rename = raise_eperm "rename";
    link = raise_eperm "link";
    chmod = raise_eperm "chmod";
    chown = raise_eperm "chown";
    truncate = raise_eperm "truncate";
    utime = raise_eperm "utime";
    setxattr = raise_eperm "setxattr";
    removexattr = raise_eperm "removexattr";
  }

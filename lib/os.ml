(** A Linux user-mode process environment, like [qemu-user]: the initial stack
    (argc, argv, envp, auxv), file descriptors backed by host files,
    [brk]/[mmap] memory management, and the RV64 Linux syscall ABI.

    Images built by riscvsim's own assembler also get RARS-style debug calls
    (print_int and friends). Those are enabled by the marker symbol
    [__riscvsim_debug_calls], because some of their numbers (34, 36) collide
    with real Linux syscalls ([mkdirat], [symlinkat]). *)

(* {1 Host I/O for the console} *)

type io = {
  out : string -> unit;  (** fd 1 *)
  err : string -> unit;  (** fd 2 *)
  input : int -> string;  (** fd 0: read up to [n] bytes, [""] at EOF *)
}

let std_io =
  {
    out = (fun s -> print_string s; flush stdout);
    err = (fun s -> prerr_string s; flush stderr);
    input =
      (fun n ->
        let b = Bytes.create n in
        Bytes.sub_string b 0 (input stdin b 0 n));
  }
[@@ocamlformat "disable"]

let buffer_io ?(input = "") buf =
  let pending = ref input in
  {
    out = Buffer.add_string buf;
    err = Buffer.add_string buf;
    input =
      (fun n ->
        let k = min n (String.length !pending) in
        let s = String.sub !pending 0 k in
        pending := String.sub !pending k (String.length !pending - k);
        s);
  }

(* {1 Process state} *)

type file = Stdin | Stdout | Stderr | Host of Unix.file_descr

type t = {
  machine : Machine.t;  (** one hart on a user-mode bus *)
  hart : Hart.t;
  io : io;
  fds : (int, file) Hashtbl.t;
  brk_start : int64;
  mutable brk : int64;
  mutable mmap_next : int64;
  mutable random : int64;  (** state for [getrandom] (deterministic) *)
  exe : string;  (** what /proc/self/exe resolves to *)
  debug_calls : bool;
  strace : (string -> unit) option;
  tohost : int64 option;  (** HTIF [tohost] address, for bare-metal tests *)
}

let stack_top = 0x3f_ffff_f000L
let mmap_base = 0x20_0000_0000L
let page_size = 4096L
let page_align a = Int64.(logand (add a 4095L) (lognot 4095L))

(* errno values (asm-generic) *)
let enoent = 2
and eio = 5
and ebadf = 9
and eagain = 11
and enomem = 12
and eacces = 13
and eexist = 17
and enotdir = 20
and eisdir = 21
and einval = 22
and enotty = 25
and espipe = 29
and enosys = 38
and enotempty = 39

let errno_of_unix : Unix.error -> int = function
  | ENOENT -> enoent
  | EBADF -> ebadf
  | EACCES | EPERM -> eacces
  | EEXIST -> eexist
  | ENOTDIR -> enotdir
  | EISDIR -> eisdir
  | EINVAL -> einval
  | ESPIPE -> espipe
  | ENOMEM -> enomem
  | ENOTEMPTY -> enotempty
  | EAGAIN -> eagain
  | _ -> eio

exception Errno of int

let fail e = raise (Errno e)

(* {1 Initial stack} *)

(* auxv keys *)
let at_null = 0
and at_phdr = 3
and at_phent = 4
and at_phnum = 5
and at_pagesz = 6
and at_base = 7
and at_flags = 8
and at_entry = 9
and at_uid = 11
and at_euid = 12
and at_gid = 13
and at_egid = 14
and at_hwcap = 16
and at_clktck = 17
and at_secure = 23
and at_random = 25
and at_execfn = 31

(** HWCAP: one bit per single-letter extension, IMAFDC. *)
let hwcap =
  List.fold_left
    (fun acc c -> acc lor (1 lsl (Char.code c - Char.code 'A')))
    0
    [ 'I'; 'M'; 'A'; 'F'; 'D'; 'C' ]

(** Build the Linux initial process stack:
    {v
      sp -> argc
            argv[0..argc-1], NULL
            envp[0..], NULL
            auxv (key, value) pairs, AT_NULL
            ... 16 random bytes, argument and environment strings
    v} *)
let setup_stack (hart : Hart.t) (img : Image.t) ~argv ~env =
  let mem = hart.mem in
  let top = ref stack_top in
  let push_bytes s =
    top := Int64.sub !top (Int64.of_int (String.length s));
    Memory.blit_bytes mem !top (Bytes.of_string s);
    !top
  in
  let push_string s = push_bytes (s ^ "\000") in
  let env_ptrs = List.rev_map push_string (List.rev env) in
  let argv_ptrs = List.rev_map push_string (List.rev argv) in
  let execfn = match argv_ptrs with p :: _ -> p | [] -> 0L in
  let random = push_bytes "riscvsim-random!" in
  let phdr, phnum, phent =
    match img.phdrs with
    | Some p -> (p.addr, p.num, p.entsize)
    | None -> (0L, 0, 0)
  in
  let auxv =
    [ (at_phdr, phdr); (at_phent, Int64.of_int phent); (at_phnum, Int64.of_int phnum);
      (at_pagesz, page_size); (at_base, 0L); (at_flags, 0L); (at_entry, img.entry);
      (at_uid, 1000L); (at_euid, 1000L); (at_gid, 1000L); (at_egid, 1000L);
      (at_hwcap, Int64.of_int hwcap); (at_clktck, 100L); (at_secure, 0L);
      (at_random, random); (at_execfn, execfn); (at_null, 0L) ]
    [@ocamlformat "disable"]
  in
  let words =
    (Int64.of_int (List.length argv) :: argv_ptrs)
    @ [ 0L ] @ env_ptrs @ [ 0L ]
    @ List.concat_map (fun (k, v) -> [ Int64.of_int k; v ]) auxv
  in
  let size = Int64.of_int (8 * List.length words) in
  let sp = Int64.logand (Int64.sub !top size) (-16L) in
  List.iteri
    (fun k w -> Memory.store mem (Int64.add sp (Int64.of_int (8 * k))) 8 w)
    words;
  Hart.set hart Reg.sp sp

(* {1 Guest memory helpers} *)

let read_string t addr = Memory.read_cstring t.hart.mem addr
let read_bytes t addr len = Memory.read_bytes t.hart.mem addr len
let write_bytes t addr b = Memory.blit_bytes t.hart.mem addr b
let write_u64 t addr v = Memory.store t.hart.mem addr 8 v
let write_u32 t addr v = Memory.store t.hart.mem addr 4 (Int64.of_int v)
let zero t addr len = write_bytes t addr (Bytes.make len '\000')

(** Drop whole pages in [addr, addr+len): they read as zero again. *)
let unmap t addr len =
  let first = Int64.shift_right_logical (page_align addr) 12 in
  let last = Int64.shift_right_logical (Int64.add addr (Int64.of_int len)) 12 in
  let rec go p =
    if Int64.compare p last < 0 then (
      Memory.unmap_page t.hart.mem (Int64.to_int p);
      go (Int64.succ p))
  in
  go first

(* {1 Files} *)

let at_fdcwd = -100

let file t fd =
  match Hashtbl.find_opt t.fds fd with Some f -> f | None -> fail ebadf

let host t fd = match file t fd with Host h -> h | _ -> fail espipe

let alloc_fd t f =
  let rec go n = if Hashtbl.mem t.fds n then go (n + 1) else n in
  let fd = go 0 in
  Hashtbl.replace t.fds fd f;
  fd

let path_of t ~dirfd path =
  if dirfd <> at_fdcwd && (String.length path = 0 || path.[0] <> '/') then
    fail enosys;
  if path = "/proc/self/exe" then t.exe else path

let open_flags flags =
  let open Unix in
  let acc =
    match flags land 3 with
    | 0 -> [ O_RDONLY ]
    | 1 -> [ O_WRONLY ]
    | _ -> [ O_RDWR ]
  in
  let bit b f acc = if flags land b <> 0 then f :: acc else acc in
  acc |> bit 0o100 O_CREAT |> bit 0o200 O_EXCL |> bit 0o1000 O_TRUNC
  |> bit 0o2000 O_APPEND |> bit 0o4000 O_NONBLOCK |> bit 0o2000000 O_CLOEXEC

(** Fill an asm-generic [struct stat] (128 bytes). *)
let write_stat t addr (st : Unix.LargeFile.stats) =
  let kind =
    match st.st_kind with
    | S_REG -> 0o100000
    | S_DIR -> 0o040000
    | S_CHR -> 0o020000
    | S_BLK -> 0o060000
    | S_LNK -> 0o120000
    | S_FIFO -> 0o010000
    | S_SOCK -> 0o140000
    [@ocamlformat "disable"]
  in
  let time f =
    (Int64.of_float f, Int64.of_float ((f -. Float.trunc f) *. 1e9))
  in
  zero t addr 128;
  let w8 off v = write_u64 t (Int64.add addr (Int64.of_int off)) v in
  let w4 off v = write_u32 t (Int64.add addr (Int64.of_int off)) v in
  w8 0 (Int64.of_int st.st_dev);
  w8 8 (Int64.of_int st.st_ino);
  w4 16 (kind lor st.st_perm);
  w4 20 st.st_nlink;
  w4 24 st.st_uid;
  w4 28 st.st_gid;
  w8 32 (Int64.of_int st.st_rdev);
  w8 48 st.st_size;
  w4 56 4096;
  w8 64 (Int64.div (Int64.add st.st_size 511L) 512L);
  List.iter
    (fun (off, f) ->
      let s, ns = time f in
      w8 off s;
      w8 (off + 8) ns)
    [ (72, st.st_atime); (88, st.st_mtime); (104, st.st_ctime) ]

let console_stat t addr ~rdev =
  zero t addr 128;
  write_u32 t (Int64.add addr 16L) 0o020620;
  write_u32 t (Int64.add addr 20L) 1;
  write_u64 t (Int64.add addr 32L) rdev;
  write_u32 t (Int64.add addr 56L) 1024

(** Read up to [len] bytes, stopping early only at end of file. [Unix.read]
    returns at most 64 KiB per call, while Linux returns the full count for
    regular files, and programs (ocamlrun among them) rely on that. *)
let read_fully h len =
  let b = Bytes.create len in
  let rec go off =
    let n = if off < len then Unix.read h b off (len - off) else 0 in
    if n > 0 then go (off + n) else off
  in
  Bytes.sub_string b 0 (go 0)

let do_read t fd buf len =
  let data =
    match file t fd with
    | Stdin -> t.io.input len
    | Stdout | Stderr -> fail ebadf
    | Host h -> read_fully h len
  in
  write_bytes t buf (Bytes.of_string data);
  String.length data

let do_write t fd data =
  (match file t fd with
  | Stdout -> t.io.out data
  | Stderr -> t.io.err data
  | Stdin -> fail ebadf
  | Host h -> ignore (Unix.write_substring h data 0 (String.length data)));
  String.length data

let iovecs t iov cnt =
  List.init cnt (fun k ->
      let e = Int64.add iov (Int64.of_int (16 * k)) in
      ( Memory.load t.hart.mem e 8,
        Int64.to_int (Memory.load t.hart.mem (Int64.add e 8L) 8) ))

let utsname =
  [ "Linux"; "riscvsim"; "6.6.0-riscvsim"; "#1 SMP"; "riscv64"; "(none)" ]

let next_random t =
  (* xorshift64*: deterministic, so runs are reproducible *)
  let open Int64 in
  let x = t.random in
  let x = logxor x (shift_right_logical x 12) in
  let x = logxor x (shift_left x 25) in
  let x = logxor x (shift_right_logical x 27) in
  t.random <- x;
  mul x 0x2545F4914F6CDD1DL

(* {1 System calls}

   Each syscall is a small function from the process and its argument
   registers to the value returned in a0. Failures raise [Errno] (or a
   [Unix_error]), and the dispatcher turns them into [-errno]. *)

type args = int -> int64
(** argument register a0..a5 *)

let int_arg (a : args) n = Int64.to_int (a n)
let i64 = Int64.of_int
let dup_file = function Host h -> Host (Unix.dup h) | f -> f

let path t (a : args) ~dirfd_arg ~path_arg =
  path_of t ~dirfd:(int_arg a dirfd_arg) (read_string t (a path_arg))

let write_timespec t addr ~divisor seconds =
  write_u64 t addr (Int64.of_float seconds);
  write_u64 t (Int64.add addr 8L)
    (Int64.of_float ((seconds -. Float.trunc seconds) *. divisor))

(* {2 Files} *)

let sys_getcwd t a =
  let cwd = Sys.getcwd () ^ "\000" in
  if String.length cwd > int_arg a 1 then fail 34 (* ERANGE *);
  write_bytes t (a 0) (Bytes.of_string cwd);
  i64 (String.length cwd)

let sys_dup t a = i64 (alloc_fd t (dup_file (file t (int_arg a 0))))

let sys_dup3 t a =
  Hashtbl.replace t.fds (int_arg a 1) (dup_file (file t (int_arg a 0)));
  a 1

let sys_fcntl t a =
  let f = file t (int_arg a 0) in
  match int_arg a 1 with
  | 0 | 1030 (* F_DUPFD, F_DUPFD_CLOEXEC *) -> i64 (alloc_fd t (dup_file f))
  | 3 (* F_GETFL *) -> (
      match f with Stdin -> 0L | Stdout | Stderr -> 1L | Host _ -> 2L)
  | _ -> 0L

let sys_ioctl t a =
  ignore (file t (int_arg a 0));
  fail enotty (* no terminals: stdio stays fully buffered, as when piped *)

let sys_mkdirat t a =
  Unix.mkdir (path t a ~dirfd_arg:0 ~path_arg:1) (int_arg a 2);
  0L

let sys_unlinkat t a =
  let p = path t a ~dirfd_arg:0 ~path_arg:1 in
  if int_arg a 2 land 0x200 (* AT_REMOVEDIR *) <> 0 then Unix.rmdir p
  else Unix.unlink p;
  0L

let sys_renameat t a =
  Unix.rename
    (path t a ~dirfd_arg:0 ~path_arg:1)
    (path t a ~dirfd_arg:2 ~path_arg:3);
  0L

let sys_faccessat t a =
  let mode = int_arg a 2 in
  let perms =
    List.filter_map
      (fun (bit, p) -> if mode land bit <> 0 then Some p else None)
      [ (4, Unix.R_OK); (2, Unix.W_OK); (1, Unix.X_OK) ]
  in
  Unix.access
    (path t a ~dirfd_arg:0 ~path_arg:1)
    (if perms = [] then [ Unix.F_OK ] else perms);
  0L

let sys_openat t a =
  let fd =
    Unix.openfile
      (path t a ~dirfd_arg:0 ~path_arg:1)
      (open_flags (int_arg a 2))
      (int_arg a 3)
  in
  i64 (alloc_fd t (Host fd))

let sys_close t a =
  (match file t (int_arg a 0) with Host h -> Unix.close h | _ -> ());
  Hashtbl.remove t.fds (int_arg a 0);
  0L

let sys_lseek t a =
  let whence =
    match int_arg a 2 with
    | 0 -> Unix.SEEK_SET
    | 1 -> Unix.SEEK_CUR
    | 2 -> Unix.SEEK_END
    | _ -> fail einval
  in
  Unix.LargeFile.lseek (host t (int_arg a 0)) (a 1) whence

let sys_read t a = i64 (do_read t (int_arg a 0) (a 1) (int_arg a 2))

let sys_write t a =
  i64
    (do_write t (int_arg a 0)
       (Bytes.to_string (read_bytes t (a 1) (int_arg a 2))))

let sys_readv t a =
  iovecs t (a 1) (int_arg a 2)
  |> List.fold_left
       (fun total (base, len) ->
         Int64.add total (i64 (do_read t (int_arg a 0) base len)))
       0L

let sys_writev t a =
  iovecs t (a 1) (int_arg a 2)
  |> List.map (fun (base, len) -> Bytes.to_string (read_bytes t base len))
  |> String.concat ""
  |> do_write t (int_arg a 0)
  |> i64

(** Run [f] with the file position of [fd] temporarily at [offset]. *)
let at_offset t fd offset f =
  let h = host t fd in
  let saved = Unix.LargeFile.lseek h 0L Unix.SEEK_CUR in
  ignore (Unix.LargeFile.lseek h offset Unix.SEEK_SET);
  Fun.protect f ~finally:(fun () ->
      ignore (Unix.LargeFile.lseek h saved Unix.SEEK_SET))

let sys_pread64 t a = at_offset t (int_arg a 0) (a 3) (fun () -> sys_read t a)

let sys_readlinkat t a =
  let p = read_string t (a 1) in
  let target =
    if p = "/proc/self/exe" then t.exe
    else Unix.readlink (path_of t ~dirfd:(int_arg a 0) p)
  in
  let s = String.sub target 0 (min (String.length target) (int_arg a 3)) in
  write_bytes t (a 2) (Bytes.of_string s);
  i64 (String.length s)

let stat_fd t fd buf =
  match file t fd with
  | Host h -> write_stat t buf (Unix.LargeFile.fstat h)
  | Stdin | Stdout | Stderr -> console_stat t buf ~rdev:0x8800L

let sys_newfstatat t a =
  let at_empty_path = 0x1000 and at_symlink_nofollow = 0x100 in
  let flags = int_arg a 3 in
  (if read_string t (a 1) = "" && flags land at_empty_path <> 0 then
     stat_fd t (int_arg a 0) (a 2)
   else
     let p = path t a ~dirfd_arg:0 ~path_arg:1 in
     write_stat t (a 2)
       (if flags land at_symlink_nofollow <> 0 then Unix.LargeFile.lstat p
        else Unix.LargeFile.stat p));
  0L

let sys_fstat t a =
  stat_fd t (int_arg a 0) (a 1);
  0L

(* {2 Process} *)

let sys_exit t a =
  t.hart.exit_code <- Some (int_arg a 0 land 0xff);
  0L

let sys_kill t a ~signal_arg =
  let signal = int_arg a signal_arg in
  t.io.err (Printf.sprintf "riscvsim: program killed by signal %d\n" signal);
  t.hart.exit_code <- Some (128 + signal);
  0L

(** No signal is ever delivered, so report empty previous state. *)
let sys_signal_state t a ~old_arg ~size =
  if a old_arg <> 0L then zero t (a old_arg) size;
  0L

(** Single-threaded: a futex wait could never be woken. *)
let sys_futex _ a =
  if int_arg a 1 land 0x7f = 0 (* FUTEX_WAIT *) then fail eagain else 0L

let sys_sched_getaffinity t a =
  zero t (a 2) (max 8 (min (int_arg a 1) 128));
  Memory.store t.hart.mem (a 2) 1 1L (* one CPU *);
  8L

let sys_uname t a =
  List.iteri
    (fun k field ->
      let b = Bytes.make 65 '\000' in
      Bytes.blit_string field 0 b 0 (String.length field);
      write_bytes t (Int64.add (a 0) (i64 (65 * k))) b)
    utsname;
  0L

let sys_prlimit t ~resource ~old =
  if old <> 0L then (
    let cur, max =
      match resource with
      | 3 (* RLIMIT_STACK *) -> (0x800000L, -1L)
      | 7 (* RLIMIT_NOFILE *) -> (1024L, 4096L)
      | _ -> (-1L, -1L)
    in
    write_u64 t old cur;
    write_u64 t (Int64.add old 8L) max);
  0L

(* {2 Time} *)

let sys_clock_gettime t a =
  if a 1 <> 0L then write_timespec t (a 1) ~divisor:1e9 (Unix.gettimeofday ());
  0L

let sys_clock_getres t a =
  if a 1 <> 0L then write_timespec t (a 1) ~divisor:1e9 1e-9;
  0L

let sys_gettimeofday t a =
  if a 0 <> 0L then write_timespec t (a 0) ~divisor:1e6 (Unix.gettimeofday ());
  0L

let sys_times t a =
  if a 0 <> 0L then zero t (a 0) 32;
  Int64.of_int t.hart.instret

(* {2 Memory} *)

let sys_brk t a =
  let req = a 0 in
  if Int64.unsigned_compare req t.brk_start >= 0 then (
    if Int64.unsigned_compare req t.brk < 0 then
      unmap t req (Int64.to_int (Int64.sub t.brk req));
    t.brk <- req);
  t.brk

(** Reserve fresh address space (never reused, so it reads as zero). *)
let reserve t len =
  let base = t.mmap_next in
  t.mmap_next <- Int64.add base (Int64.add (page_align len) page_size)
  (* guard page *);
  base

let sys_munmap t a =
  unmap t (a 0) (int_arg a 1);
  0L

let sys_mremap t a =
  let old, old_len, new_len = (a 0, int_arg a 1, int_arg a 2) in
  let base = reserve t (i64 new_len) in
  write_bytes t base (read_bytes t old (min old_len new_len));
  unmap t old old_len;
  base

let read_file_into t fd ~offset ~addr ~len =
  at_offset t fd offset (fun () ->
      write_bytes t addr (Bytes.of_string (read_fully (host t fd) len)))

let sys_mmap t a =
  let map_fixed = 0x10 and map_anonymous = 0x20 in
  let len = page_align (a 1) and flags = int_arg a 3 in
  let base =
    if flags land map_fixed <> 0 then (
      unmap t (a 0) (Int64.to_int len);
      a 0)
    else reserve t len
  in
  if flags land map_anonymous = 0 then
    read_file_into t (int_arg a 4) ~offset:(a 5) ~addr:base
      ~len:(Int64.to_int len);
  base

let sys_getrandom t a =
  let len = int_arg a 1 in
  write_bytes t (a 0)
    (Bytes.init len (fun _ -> Char.chr (Int64.to_int (next_random t) land 0xff)));
  i64 len

(* {2 RARS-style debug calls (assembler-built images only)} *)

let print t s (a : args) =
  t.io.out s;
  a 0

let debug_calls : (int * (string * (t -> args -> int64))) list =
  [ (1, ("print_int", fun t a -> print t (Int64.to_string (a 0)) a));
    (2, ("print_float", fun t a -> print t (Fpu.string_of_single (Hart.get_s t.hart (Freg.of_int 10))) a));
    (3, ("print_double", fun t a -> print t (Fpu.string_of_double (Int64.float_of_bits (Hart.get_f t.hart 10))) a));
    (4, ("print_string", fun t a -> print t (read_string t (a 0)) a));
    (10, ("exit", fun t _ -> t.hart.exit_code <- Some 0; 0L));
    (11, ("print_char", fun t a -> print t (String.make 1 (Char.chr (int_arg a 0 land 0xff))) a));
    (34, ("print_hex", fun t a -> print t (Printf.sprintf "0x%016Lx" (a 0)) a));
    (36, ("print_unsigned", fun t a -> print t (Printf.sprintf "%Lu" (a 0)) a)) ]
[@@ocamlformat "disable"]

(* {2 The Linux table} *)

let constant v _ _ = v
let ok = constant 0L

(** Known but deliberately unsupported: callers (glibc) fall back. *)
let unsupported _ _ = fail enosys

let linux_calls : (int * (string * (t -> args -> int64))) list =
  [ (17, ("getcwd", sys_getcwd)); (23, ("dup", sys_dup)); (24, ("dup3", sys_dup3));
    (25, ("fcntl", sys_fcntl)); (29, ("ioctl", sys_ioctl)); (34, ("mkdirat", sys_mkdirat));
    (35, ("unlinkat", sys_unlinkat)); (38, ("renameat", sys_renameat));
    (48, ("faccessat", sys_faccessat)); (56, ("openat", sys_openat)); (57, ("close", sys_close));
    (62, ("lseek", sys_lseek)); (63, ("read", sys_read)); (64, ("write", sys_write));
    (65, ("readv", sys_readv)); (66, ("writev", sys_writev)); (67, ("pread64", sys_pread64));
    (78, ("readlinkat", sys_readlinkat)); (79, ("newfstatat", sys_newfstatat));
    (80, ("fstat", sys_fstat)); (82, ("fsync", ok)); (83, ("fdatasync", ok));
    (93, ("exit", sys_exit)); (94, ("exit_group", sys_exit));
    (96, ("set_tid_address", constant 1000L)); (98, ("futex", sys_futex));
    (99, ("set_robust_list", ok)); (101, ("nanosleep", ok));
    (113, ("clock_gettime", sys_clock_gettime)); (114, ("clock_getres", sys_clock_getres));
    (115, ("clock_nanosleep", ok)); (122, ("sched_setaffinity", ok));
    (123, ("sched_getaffinity", sys_sched_getaffinity)); (124, ("sched_yield", ok));
    (129, ("kill", sys_kill ~signal_arg:1)); (130, ("tkill", sys_kill ~signal_arg:1));
    (131, ("tgkill", sys_kill ~signal_arg:2));
    (132, ("sigaltstack", sys_signal_state ~old_arg:1 ~size:24));
    (134, ("rt_sigaction", sys_signal_state ~old_arg:2 ~size:32));
    (135, ("rt_sigprocmask", sys_signal_state ~old_arg:2 ~size:8));
    (153, ("times", sys_times)); (160, ("uname", sys_uname));
    (163, ("getrlimit", fun t a -> sys_prlimit t ~resource:(int_arg a 0) ~old:(a 1)));
    (165, ("getrusage", fun t a -> zero t (a 1) 144; 0L)); (167, ("prctl", ok));
    (169, ("gettimeofday", sys_gettimeofday));
    (172, ("getpid", constant 1000L)); (173, ("getppid", constant 1L));
    (174, ("getuid", constant 1000L)); (175, ("geteuid", constant 1000L));
    (176, ("getgid", constant 1000L)); (177, ("getegid", constant 1000L));
    (178, ("gettid", constant 1000L)); (214, ("brk", sys_brk)); (215, ("munmap", sys_munmap));
    (216, ("mremap", sys_mremap)); (222, ("mmap", sys_mmap)); (226, ("mprotect", ok));
    (233, ("madvise", ok)); (258, ("riscv_hwprobe", unsupported));
    (259, ("riscv_flush_icache", ok)); (293, ("rseq", unsupported));
    (261, ("prlimit64", fun t a -> sys_prlimit t ~resource:(int_arg a 1) ~old:(a 3)));
    (278, ("getrandom", sys_getrandom)) ]
[@@ocamlformat "disable"]

let linux_table = Hashtbl.of_seq (List.to_seq linux_calls)
let debug_table = Hashtbl.of_seq (List.to_seq debug_calls)

let lookup t num =
  let debug =
    if t.debug_calls then Hashtbl.find_opt debug_table num else None
  in
  match debug with Some h -> Some h | None -> Hashtbl.find_opt linux_table num

let syscall t =
  let hart = t.hart in
  let a n = Hart.get_x hart (10 + n) in
  let num = Int64.to_int (Hart.get_x hart 17) in
  let name, result =
    match lookup t num with
    | None -> (Printf.sprintf "syscall_%d" num, i64 (-enosys))
    | Some (name, handler) ->
        ( name,
          try handler t a with
          | Errno e -> i64 (-e)
          | Unix.Unix_error (e, _, _) -> i64 (-errno_of_unix e) )
  in
  Option.iter
    (fun log ->
      log
        (Printf.sprintf "%s(0x%Lx, 0x%Lx, 0x%Lx) = %Ld\n" name (a 0) (a 1) (a 2)
           result))
    t.strace;
  if hart.exit_code = None then Hart.set_x hart 10 result

(** Create a CPU running [img] as a Linux process. *)
let boot ?(io = std_io) ?(argv = [ "a.out" ]) ?(env = []) ?exe ?strace
    (img : Image.t) =
  let machine = Machine.create (Bus.user ()) in
  let hart = Machine.hart machine 0 in
  Hart.load hart img;
  setup_stack hart img ~argv ~env;
  let brk = page_align (Image.end_address img) in
  let fds = Hashtbl.create 8 in
  List.iter
    (fun (fd, f) -> Hashtbl.replace fds fd f)
    [ (0, Stdin); (1, Stdout); (2, Stderr) ];
  let t =
    {
      machine;
      hart;
      io;
      fds;
      brk_start = brk;
      brk;
      mmap_next = mmap_base;
      random = 0x9E3779B97F4A7C15L;
      exe =
        (match exe with Some e -> e | None -> List.hd (argv @ [ "a.out" ]));
      debug_calls = List.mem_assoc "__riscvsim_debug_calls" img.symbols;
      strace;
      tohost = List.assoc_opt "tohost" img.symbols;
    }
  in
  hart.env <- Emulated (fun _ -> syscall t);
  t

(* {1 Running} *)

(** HTIF exit request: bare-metal test programs (built for Spike) halt by
    storing [(code lsl 1) lor 1] to the [tohost] symbol. *)
let htif_exit t =
  Option.bind t.tohost (fun addr ->
      let v = Memory.load t.hart.mem addr 8 in
      if Int64.logand v 1L = 1L then
        Some (Int64.to_int (Int64.shift_right_logical v 1) land 0xff)
      else None)

(** Run the process until it exits, traps or uses up [fuel]. HTIF exits are
    honoured too: like Spike, [tohost] is polled between slices of instructions
    rather than on every store. *)
let run ?fuel ?trace t =
  Machine.run ?fuel ?trace ~poll:(fun () -> htif_exit t) t.machine

(** The words between [begin_signature] and [end_signature], one per line as 8
    hex digits: the riscv-arch-test signature format. *)
let signature (hart : Hart.t) (img : Image.t) =
  match
    ( List.assoc_opt "begin_signature" img.symbols,
      List.assoc_opt "end_signature" img.symbols )
  with
  | Some b, Some e ->
      let buf = Buffer.create 4096 in
      let rec go a =
        if Int64.compare a e < 0 then (
          Buffer.add_string buf
            (Printf.sprintf "%08Lx\n" (Memory.load hart.mem a 4));
          go (Int64.add a 4L))
      in
      go b;
      Some (Buffer.contents buf)
  | _ -> None

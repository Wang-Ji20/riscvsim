(** Running bare-metal programs the way Spike does: test suites (riscv-tests,
    riscv-arch-test) and other programs built for Spike.

    One hart starts in M-mode at the program's entry, on a system-mode bus with
    {!ram_size} bytes of RAM at {!ram_base}. The program stops through HTIF (see
    {!Htif}). By default traps are delivered to the program's own handlers, as
    on real hardware. *)

let ram_base = 0x8000_0000L
let ram_size = 0x400_0000L (* 64 MiB *)

(** A machine with [image] loaded and one hart at its entry point. *)
let boot ?(env = Hart.System) (image : Image.t) =
  let m = Machine.create (Bus.system ~ram_base ~ram_size) in
  let hart = Machine.hart m 0 in
  hart.env <- env;
  Hart.load hart image;
  m

(** Run a booted machine until the program exits through HTIF, traps (only with
    [env = Bare]), or uses up [fuel]. *)
let continue ?fuel ?trace (m : Machine.t) (image : Image.t) =
  let poll () =
    Option.bind (Htif.tohost image) (fun tohost ->
        Htif.exit_code (Bus.ram m.bus) ~tohost)
  in
  Machine.run ?fuel ?trace ~poll m

(** Boot and run [image]. Returns the hart for inspection. *)
let run ?env ?fuel ?trace (image : Image.t) =
  let m = boot ?env image in
  (Machine.hart m 0, continue ?fuel ?trace m image)

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

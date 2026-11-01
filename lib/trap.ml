(** Synchronous exceptions ("traps"), with their RISC-V cause codes as written
    to [mcause]/[scause] and the value written to [mtval]/[stval].

    An instruction that traps changes no architectural state: registers, [pc],
    memory, CSRs and the instruction counter are as they were before it. *)

type cause =
  | Instruction_address_misaligned
  | Instruction_access_fault
  | Illegal_instruction
  | Breakpoint
  | Load_address_misaligned
  | Load_access_fault
  | Store_address_misaligned  (** also AMOs and [sc] *)
  | Store_access_fault
  | Ecall_from_u
  | Ecall_from_s
  | Ecall_from_m
  | Instruction_page_fault
  | Load_page_fault
  | Store_page_fault

type t = { cause : cause; tval : int64 }

exception Trap of t

(** Cause code and description, in the order of the privileged spec's table. *)
let causes =
  [ (Instruction_address_misaligned, 0, "instruction address misaligned");
    (Instruction_access_fault, 1, "instruction access fault");
    (Illegal_instruction, 2, "illegal instruction");
    (Breakpoint, 3, "breakpoint");
    (Load_address_misaligned, 4, "load address misaligned");
    (Load_access_fault, 5, "load access fault");
    (Store_address_misaligned, 6, "store/AMO address misaligned");
    (Store_access_fault, 7, "store/AMO access fault");
    (Ecall_from_u, 8, "environment call from U-mode");
    (Ecall_from_s, 9, "environment call from S-mode");
    (Ecall_from_m, 11, "environment call from M-mode");
    (Instruction_page_fault, 12, "instruction page fault");
    (Load_page_fault, 13, "load page fault");
    (Store_page_fault, 15, "store/AMO page fault") ]
[@@ocamlformat "disable"]

let info cause =
  match List.find_opt (fun (c, _, _) -> c = cause) causes with
  | Some (_, code, name) -> (code, name)
  | None -> assert false (* [causes] lists every constructor *)

let code cause = fst (info cause)
let name cause = snd (info cause)
let raise_trap cause tval = raise (Trap { cause; tval })

(** The environment-call cause for each privilege level. *)
let ecall_from : Priv.t -> cause = function
  | U -> Ecall_from_u
  | S -> Ecall_from_s
  | M -> Ecall_from_m

let pp ppf { cause; tval } =
  Format.fprintf ppf "%s (cause %d, tval 0x%Lx)" (name cause) (code cause) tval

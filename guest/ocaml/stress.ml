(* A workout for the OCaml runtime: GC, floats, exceptions, I/O, effects. *)

let section name = Printf.printf "== %s\n" name

let gc () =
  section "gc";
  (* allocate lots of short- and long-lived data to force minor/major GCs *)
  let keep = ref [] in
  for i = 1 to 200_000 do
    let l = List.init 10 (fun j -> i + j) in
    if i mod 1000 = 0 then keep := l :: !keep
  done;
  Gc.full_major ();
  Printf.printf "kept %d lists, sum %d\n" (List.length !keep)
    (List.fold_left (fun a l -> a + List.fold_left ( + ) 0 l) 0 !keep)

let floats () =
  section "floats";
  List.iter
    (fun x -> Printf.printf "%.17g %h %e %s\n" x x x (string_of_float x))
    [ 0.1; 1. /. 3.; Float.pi; 1e-310; Float.max_float; sqrt 2.; exp 1.; -0. ];
  Printf.printf "%.3f %.10f %g\n" (atan2 1. 2.) (Float.fma 0.1 10. (-1.))
    (Float.of_string "6.02e23")

let tables () =
  section "hashtables and maps";
  let h = Hashtbl.create 16 in
  String.split_on_char ' ' "the quick brown fox jumps over the lazy dog the end"
  |> List.iter (fun w ->
      Hashtbl.replace h w (1 + Option.value ~default:0 (Hashtbl.find_opt h w)));
  let module M = Map.Make (String) in
  let m = Hashtbl.fold M.add h M.empty in
  M.iter (fun k v -> Printf.printf "%s=%d " k v) m;
  print_newline ()

exception Found of int

let exceptions () =
  section "exceptions";
  let find p l =
    try
      List.iter (fun x -> if p x then raise (Found x)) l;
      None
    with Found x -> Some x
  in
  Printf.printf "%s\n"
    (match find (fun x -> x * x > 50) (List.init 20 Fun.id) with
    | Some x -> string_of_int x
    | None -> "none");
  (try ignore (List.nth [] 3)
   with Failure m | Invalid_argument m -> Printf.printf "caught: %s\n" m);
  Printf.printf "stack depth ok: %d\n"
    (let rec deep n = if n = 0 then 0 else 1 + deep (n - 1) in
     deep 100_000)

let files () =
  section "files";
  let path =
    Filename.concat (Filename.get_temp_dir_name ()) "riscvsim-ocaml-test.txt"
  in
  Out_channel.with_open_text path (fun oc ->
      for i = 1 to 3 do
        Printf.fprintf oc "line %d\n" i
      done);
  let lines = In_channel.with_open_text path In_channel.input_lines in
  Sys.remove path;
  Printf.printf "%d lines: %s; exists after remove: %b\n" (List.length lines)
    (String.concat "|" lines) (Sys.file_exists path)

type _ Effect.t += Ask : int Effect.t

let effects () =
  section "effects";
  let open Effect.Deep in
  let result =
    try_with
      (fun () -> Effect.perform Ask + Effect.perform Ask)
      ()
      {
        effc =
          (fun (type a) (e : a Effect.t) ->
            match e with
            | Ask -> Some (fun (k : (a, _) continuation) -> continue k 21)
            | _ -> None);
      }
  in
  Printf.printf "effect handler result: %d\n" result

let () =
  Printf.printf "argv: %s\n"
    (String.concat " " (List.tl (Array.to_list Sys.argv)));
  gc ();
  floats ();
  tables ();
  exceptions ();
  files ();
  effects ();
  Format.printf "@[<v 2>format:@ %a@]@."
    (Format.pp_print_list Format.pp_print_int)
    [ 1; 2; 3 ];
  exit 7

(* Algebraic Effects Test (OCaml 5.x) *)

open Effect
open Effect.Deep

type _ Effect.t += Ask : string Effect.t

let test_basic () =
  print_string "--- Basic Effects ---"; print_newline ();
  let h =
    try_with (fun () -> perform Ask) ()
    { effc = fun (type a) (e : a t) ->
        match e with
        | Ask -> Some (fun (k : (a, _) continuation) ->
            print_string "Effect Ask performed"; print_newline ();
            continue k "OCaml 5")
        | _ -> None
    }
  in
  print_string "Result: "; print_string h; print_newline ()

type _ Effect.t += Yield : unit Effect.t

let test_loop () =
  print_string "--- Effect Loop ---"; print_newline ();
  let count = ref 0 in
  try_with (fun () ->
    for i = 1 to 3 do
      perform Yield
    done) ()
  { effc = fun (type a) (e : a t) ->
      match e with
      | Yield -> Some (fun (k : (a, _) continuation) ->
          incr count;
          print_string "Yield "; print_int !count; print_newline ();
          continue k ())
      | _ -> None
  }

let () =
  try
    test_basic ();
    test_loop ()
  with e ->
    print_string "Error: "; print_string (Printexc.to_string e); print_newline ()

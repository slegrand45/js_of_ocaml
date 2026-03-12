type _ Effect.t += Hello : string -> unit Effect.t

let f () =
  Effect.perform (Hello "World")

let () =
  Effect.Deep.match_with f ()
  { effc = (fun (type a) (e : a Effect.t) ->
      match e with
      | Hello s -> Some (fun (k : (a, _) Effect.Deep.continuation) ->
          print_endline ("Hello " ^ s);
          Effect.Deep.continue k ())
      | _ -> None);
    exnc = (fun e -> raise e);
    retc = (fun x -> x) }

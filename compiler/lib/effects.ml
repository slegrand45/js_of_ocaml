(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* The following CPS transform is based on the one proposed in D.
   Hillerström, S. Lindley, R. Atkey, and K. C. Sivaramakrishnan,
   “Continuation Passing Style for Effect Handlers” (FSCD 2017), with
   adaptations to account for exception handlers (which are not
   considered in detail in the paper) and for the fact that the
   language is an SSA form rather than a classical lambda calculus.

   Rather than using a stack of continuations, and effect and
   exception handlers, only the current continuation is passed between
   functions, while exception handlers and effect handlers are stored
   in global variables. This avoid having to manipulate the stack each
   time the current continuation changes. This also allows us to deal
   with exceptions from the runtime or from JavaScript code (a [try
   ... with] at the top of stack can have access to the current
   exception handler and resume the execution from there; see the
   definition of runtime function [caml_callback]).
*)
open! Stdlib
open Code

let debug = Debug.find "effects"

let double_translate () =
  match Config.effects () with
  | `Disabled | `Jspi -> assert false
  | `Cps -> false
  | `Double_translation -> true
  | `Fibers -> false

let get_edges g src = Addr.Hashtbl.find_opt g src |> Option.value ~default:Addr.Set.empty

let add_edge g src dst = Addr.Hashtbl.replace g src (Addr.Set.add dst (get_edges g src))

let reverse_graph g =
  let g' = Addr.Hashtbl.create 16 in
  Addr.Hashtbl.iter
    (fun child parents -> Addr.Set.iter (fun parent -> add_edge g' parent child) parents)
    g;
  g'

type control_flow_graph =
  { succs : Addr.Set.t Addr.Hashtbl.t
  ; _preds : Addr.Set.t Addr.Hashtbl.t
  ; reverse_post_order : Addr.t list
  ; block_order : int Addr.Hashtbl.t
  }

let build_graph blocks pc =
  let succs = Addr.Hashtbl.create 16 in
  let l = ref [] in
  let visited = Addr.Hashtbl.create 16 in
  let rec traverse pc =
    if not (Addr.Hashtbl.mem visited pc)
    then (
      Addr.Hashtbl.add visited pc ();
      let successors = Code.fold_children blocks pc Addr.Set.add Addr.Set.empty in
      Addr.Hashtbl.add succs pc successors;
      Addr.Set.iter traverse successors;
      l := pc :: !l)
  in
  traverse pc;
  let block_order = Addr.Hashtbl.create 16 in
  List.iteri !l ~f:(fun i pc -> Addr.Hashtbl.add block_order pc i);
  let preds = reverse_graph succs in
  { succs; _preds = preds; reverse_post_order = !l; block_order }

let _dominator_tree g =
  (* A Simple, Fast Dominance Algorithm
     Keith D. Cooper, Timothy J. Harvey, and Ken Kennedy *)
  let dom = Addr.Hashtbl.create 16 in
  let rec inter pc pc' =
    (* Compute closest common ancestor *)
    if pc = pc'
    then pc
    else if Addr.Hashtbl.find g.block_order pc < Addr.Hashtbl.find g.block_order pc'
    then inter pc (Addr.Hashtbl.find dom pc')
    else inter (Addr.Hashtbl.find dom pc) pc'
  in
  List.iter g.reverse_post_order ~f:(fun pc ->
      let l = Addr.Hashtbl.find g.succs pc in
      Addr.Set.iter
        (fun pc' ->
          let d =
            match Addr.Hashtbl.find_opt dom pc' with
            | None -> pc
            | Some d -> inter pc d
          in
          Addr.Hashtbl.replace dom pc' d)
        l);
  dom

let _immediate_dominator p pc =
  let g = build_graph p.blocks pc in
  _dominator_tree g

(****)

let empty_body body = List.is_empty body

(****)

let is_prefix ~prefix s = StringLabels.starts_with ~prefix s

let is_primitive_name s =
  is_prefix ~prefix:"caml_cps_" s
  || is_prefix ~prefix:"caml_trampoline_" s
  || is_prefix ~prefix:"caml_exact_trampoline_" s

let _is_primitive = function
  | Pv v -> (
      match Var.get_name v with
      | None -> false
      | Some s -> is_primitive_name s)
  | _ -> false

let _is_application = function
  | Apply _ -> true
  | _ -> false

let effect_primitive_or_application = function
  | Prim (Extern ("%perform" | "%reperform" | "%resume"), _) -> true
  | Apply _ -> true
  | _ -> false

(****)

let rewrite_toplevel ~cps_needed (p : program) =
  let p, cps_needed =
    match Addr.Map.find_opt p.start p.blocks with
    | None -> p, cps_needed
    | Some block ->
        let f = Var.fresh_n "toplevel" in
        let pc = p.free_pc in
        let p =
          { blocks = Addr.Map.add pc { block with params = [] } p.blocks
          ; start = pc
          ; free_pc = pc + 1
          }
        in
        p, Var.Set.add f cps_needed
  in
  p, cps_needed

(****)

type state =
  { in_cps : Var.Set.t ref
  ; cps_needed : Var.Set.t
  ; blocks_to_transform : Addr.Set.t
  ; closure_info : (Var.t list * cont) Addr.Hashtbl.t
  ; is_continuation : Var.t Addr.Hashtbl.t
  ; matching_exn_handler : Addr.t Addr.Hashtbl.t
  }

type trampolined_calls = Var.Set.t

type in_cps = Var.Set.t

let mark_continuations ~cps_needed (p : program) =
  let is_continuation = Addr.Hashtbl.create 16 in
  let mark_continuation pc x =
    match Addr.Hashtbl.find_opt is_continuation pc with
    | None -> Addr.Hashtbl.add is_continuation pc x
    | Some x' -> assert (Var.equal x x')
  in
  Addr.Map.iter
    (fun _pc block ->
      match block.branch with
      | Pushtrap (_, x, (handler_pc, _)) -> mark_continuation handler_pc x
      | _ -> ())
    p.blocks;
  let blocks_to_transform = ref Addr.Set.empty in
  let rec mark pc =
    if not (Addr.Set.mem pc !blocks_to_transform)
    then (
      blocks_to_transform := Addr.Set.add pc !blocks_to_transform;
      let block = Addr.Map.find pc p.blocks in
      let visit_cont (pc, _) = mark pc in
      match block.branch with
      | Pushtrap (k_body, _, k_handler) when pc <> fst k_handler ->
          visit_cont k_body;
          visit_cont k_handler
      | Return _ | Stop | Raise _ -> ()
      | Branch k | Poptrap k -> visit_cont k
      | Cond (_, k1, k2) ->
          visit_cont k1;
          visit_cont k2
      | Switch (_, ks) -> Array.iter ks ~f:visit_cont
      | Pushtrap _ -> ())
  in
  let visit_block pc block =
    let has_cps_call =
      List.exists block.body ~f:(function
          | Let (x, e) when effect_primitive_or_application e && Var.Set.mem x cps_needed ->
              true
          | _ -> false)
    in
    if has_cps_call then mark pc;
    match block.branch with
    | Return x when Var.Set.mem x cps_needed -> mark pc
    | _ -> ()
  in
  Addr.Map.iter visit_block p.blocks;
  !blocks_to_transform, is_continuation

let closure_info ~cps_needed (p : program) =
  let closure_info = Addr.Hashtbl.create 16 in
  let visit_instr = function
    | Let (x, Closure (params, cont, _)) when Var.Set.mem x cps_needed ->
        Addr.Hashtbl.add closure_info (fst cont) (params, cont)
    | _ -> ()
  in
  Addr.Map.iter (fun _ block -> List.iter block.body ~f:visit_instr) p.blocks;
  closure_info

let matching_exn_handler (p : program) =
  let matching_exn_handler = Addr.Hashtbl.create 16 in
  let rec visit pc handler_pc =
    if not (Addr.Hashtbl.mem matching_exn_handler pc)
    then (
      Addr.Hashtbl.add matching_exn_handler pc handler_pc;
      let block = Addr.Map.find pc p.blocks in
      match block.branch with
      | Pushtrap (body_cont, _, _) -> visit (fst body_cont) handler_pc
      | Poptrap cont -> visit (fst cont) handler_pc
      | Return _ | Stop | Raise _ -> ()
      | Branch k | Cond (_, k, _) -> visit (fst k) handler_pc
      | Switch (_, ks) -> visit (fst ks.(0)) handler_pc)
  in
  Addr.Map.iter
    (fun _ block ->
      match block.branch with
      | Pushtrap (body_cont, _, handler_cont) -> visit (fst body_cont) (fst handler_cont)
      | _ -> ())
    p.blocks;
  matching_exn_handler

(****)

let cps_jump_cont ~st ~src:_ (pc, args) =
  match Addr.Set.mem pc st.blocks_to_transform with
  | false -> pc, args
  | true ->
      let k =
        match Addr.Hashtbl.find_opt st.is_continuation pc with
        | None -> (
            match Addr.Hashtbl.find_opt st.closure_info pc with
            | None ->
                (* This case can happen for the entry point of a
                 function that is not called as a closure (for
                 instance the toplevel) *)
                Var.fresh_n "k"
            | Some (_, (_, k_args)) -> List.last k_args |> Option.get)
        | Some k -> k
      in
      pc, args @ [ k ]

let cps_branch ~st ~src (pc, args) =
  let pc, args = cps_jump_cont ~st ~src (pc, args) in
  [], Branch (pc, args)

let cps_cont_of_direct ~st (pc, args) =
  match Addr.Set.mem pc st.blocks_to_transform with
  | false -> pc, args
  | true ->
      (* This case can happen for a [Pushtrap] where the handler is
         transformed but the body is not (or the other way around) *)
      let k = Var.fresh_n "k" in
      pc, args @ [ k ]

let allocate_continuation ~st ~alloc_jump_closures ~split_closures _pc _x (handler_pc, args) =
  let k = Var.fresh_n "k" in
  let k_params, (k_pc, k_args) = Addr.Hashtbl.find st.closure_info handler_pc in
  let k_args =
    List.map k_args ~f:(fun v ->
        if List.exists k_params ~f:(Var.equal v) then v else Var.fork v)
  in
  let k_val = Var.fresh_n "k_val" in
  let args =
    List.map2 k_args (args @ [ k ]) ~f:(fun v v' ->
        if List.exists k_params ~f:(Var.equal v) then v' else v)
  in
  let closure = Let (k_val, Closure (k_params, (k_pc, args), None)) in
  let instr, k_val =
    if split_closures
    then (
      let k_val' = Var.fresh_n "k_val" in
      ( [ closure; Let (k_val', Prim (Extern "caml_curry_continuation", [ Pv k_val ])) ]
      , k_val' ))
    else [ closure ], k_val
  in
  alloc_jump_closures @ instr, k_val

let tail_call ~st ~pc ~instrs ~exact ~in_cps ~check ~f args =
  let k =
    match Addr.Hashtbl.find_opt st.is_continuation pc with
    | None -> (
        match Addr.Hashtbl.find_opt st.closure_info pc with
        | None -> Var.fresh_n "k"
        | Some (_, (_, k_args)) -> List.last k_args |> Option.get)
    | Some k -> k
  in
  let instrs, f =
    if check
    then (
      let f' = Var.fresh_n "f" in
      let check = Let (Var.fresh (), Prim (Extern "caml_stack_check_depth", [])) in
      check :: instrs @ [ Let (f', Prim (Extern "caml_trampoline_return", [ Pv f ])) ], f')
    else instrs, f
  in
  if in_cps then st.in_cps := Var.Set.add f !(st.in_cps);
  let res = Var.fresh () in
  instrs @ [ Let (res, Apply { f; args = args @ [ k ]; exact }) ], Return res

let rewrite_last ~st pc (alloc_jump_closures : instr list) (last : last) =
  match last with
  | Return x -> (
      match Var.Set.mem x st.cps_needed, Addr.Set.mem pc st.blocks_to_transform with
      | false, false -> alloc_jump_closures, Return x
      | false, true ->
          let k =
            match Addr.Hashtbl.find_opt st.is_continuation pc with
            | None -> (
                match Addr.Hashtbl.find_opt st.closure_info pc with
                | None -> Var.fresh_n "k"
                | Some (_, (_, k_args)) -> List.last k_args |> Option.get)
            | Some k -> k
          in
          let res = Var.fresh () in
          alloc_jump_closures @ [ Let (res, Apply { f = k; args = [ x ]; exact = true }) ]
          , Return res
      | true, _ ->
          (* Function returning a CPS function *)
          alloc_jump_closures, Return x)
  | Raise (x, m) -> (
      match Addr.Set.mem pc st.blocks_to_transform with
      | false -> alloc_jump_closures, Raise (x, m)
      | true ->
          let exn_handler = Var.fresh_n "exn_handler" in
          let x', instrs =
            match m with
            | `Notrace -> x, []
            | `Normal | `Reraise ->
                let x' = Var.fresh_n "exn" in
                let force =
                  match m with
                  | `Normal -> true
                  | `Reraise -> false
                  | `Notrace -> assert false
                in
                let i =
                  [ Let
                      ( x'
                      , Prim
                          ( Extern "caml_maybe_attach_backtrace"
                          , [ Pv x
                            ; Pc (Int (if force then Targetint.one else Targetint.zero))
                            ] ) )
                  ]
                in
                x', i
          in
          let body, branch =
            tail_call
              ~st
              ~pc
              ~instrs:(Let (exn_handler, Prim (Extern "caml_pop_trap", [])) :: instrs)
              ~exact:true
              ~in_cps:false
              ~check:false
              ~f:exn_handler
              [ x' ]
          in
          alloc_jump_closures @ body, branch)
  | Stop ->
      assert (List.is_empty alloc_jump_closures);
      [], Stop
  | Branch cont ->
      let body, branch = cps_branch ~st ~src:pc cont in
      alloc_jump_closures @ body, branch
  | Cond (x, cont1, cont2) ->
      ( alloc_jump_closures
      , Cond (x, cps_jump_cont ~st ~src:pc cont1, cps_jump_cont ~st ~src:pc cont2) )
  | Switch (x, c1) ->
      (* To avoid code duplication during JavaScript generation, we need
         to create a single block per continuation *)
      let cps_jump_cont = Fun.memoize (fun x -> cps_jump_cont ~st ~src:pc x) in
      alloc_jump_closures, Switch (x, Array.map c1 ~f:cps_jump_cont)
  | Pushtrap (body_cont, exn, ((handler_pc, _) as handler_cont)) -> (
      assert (Addr.Hashtbl.mem st.is_continuation handler_pc);
      match Addr.Set.mem handler_pc st.blocks_to_transform with
      | false ->
          let body_cont = cps_cont_of_direct ~st body_cont in
          let handler_cont = cps_cont_of_direct ~st handler_cont in
          let last = Pushtrap (body_cont, exn, handler_cont) in
          alloc_jump_closures, last
      | true ->
          let constr_cont, exn_handler =
            allocate_continuation
              ~st
              ~alloc_jump_closures
              ~split_closures:true
              pc
              exn
              handler_cont
          in
          let push_trap =
            Let (Var.fresh (), Prim (Extern "caml_push_trap", [ Pv exn_handler ]))
          in
          let body, branch = cps_branch ~st ~src:pc body_cont in
          constr_cont @ (push_trap :: body), branch)
  | Poptrap cont -> (
      match
        Addr.Set.mem (Addr.Hashtbl.find st.matching_exn_handler pc) st.blocks_to_transform
      with
      | false -> alloc_jump_closures, Poptrap (cps_jump_cont ~st ~src:pc cont)
      | true ->
          let exn_handler = Var.fresh () in
          let body, branch = cps_branch ~st ~src:pc cont in
          ( alloc_jump_closures
            @ (Let (exn_handler, Prim (Extern "caml_pop_trap", [])) :: body)
          , branch ))

let rewrite_instr ~st (instr : instr) : instr =
  match instr with
  | Let (x, Closure (_, (pc, _), _)) when Var.Set.mem x st.cps_needed ->
      (* When CPS-transforming with double translation enabled, there are no closures in
         code that requires transforming, due to lambda lifting. *)
      assert (not (double_translate ()));
      (* Add the continuation parameter, and change the initial block if
         needed *)
      let cps_params, cps_cont = Addr.Hashtbl.find st.closure_info pc in
      st.in_cps := Var.Set.add x !(st.in_cps);
      Let (x, Closure (cps_params, cps_cont, None))
  | Let (x, Prim (Extern "caml_alloc_dummy_function", [ size; arity ])) -> (
      (* Removed in OCaml 5.2 *)
      match arity with
      | Pc (Int a) ->
          Let
            ( x
            , Prim
                (Extern "caml_alloc_dummy_function", [ size; Pc (Int (Targetint.succ a)) ])
            )
      | _ -> assert false)
  | Let (x, Apply { f; args; exact }) when not (Var.Set.mem x st.cps_needed) ->
      if double_translate ()
      then
        let exact =
          (* If this function is unknown to the global flow analysis, then it was
           introduced by the lambda lifting and we don't have exactness info any more. *)
          false
        in
        Let (x, Apply { f; args; exact })
      else Let (x, Apply { f; args; exact })
  | Let (x, Apply { f; args; exact }) ->
      if double_translate ()
      then
        let k = Var.fresh_n "k" in
        let exact = false in
        Let (x, Apply { f; args = args @ [ k ]; exact })
      else
        let k = Var.fresh_n "k" in
        Let (x, Apply { f; args = args @ [ k ]; exact })
  | Let (x, Prim (p, args)) -> Let (x, Prim (p, args))
  | Let (x, Constant c) -> Let (x, Constant c)
  | Let (x, Block (tag, vars, a, m)) -> Let (x, Block (tag, vars, a, m))
  | Let (x, Field (v, i, ft)) -> Let (x, Field (v, i, ft))
  | Let (x, Special s) -> Let (x, Special s)
  | Let (x, Closure (params, cont, loc)) -> Let (x, Closure (params, cont, loc))
  | Assign (v1, v2) -> Assign (v1, v2)
  | Set_field (v1, i, ft, v2) -> Set_field (v1, i, ft, v2)
  | Offset_ref (v, i) -> Offset_ref (v, i)
  | Array_set (v1, v2, v3) -> Array_set (v1, v2, v3)
  | Event loc -> Event loc

let rewrite_instrs ~st (pc, block) =
  let body, last =
    let rec rewrite accu = function
      | [] -> rewrite_last ~st pc (List.rev accu) block.branch
      | (Let (x, Prim (Extern "%resume", [ Pv stack; Pv f; Pv arg; tail ])) as i) :: r -> (
          match Var.Set.mem x st.cps_needed with
          | false -> rewrite (i :: accu) r
          | true ->
              let k = Var.fresh_n "k" in
              let k' = Var.fresh_n "k" in
              let body, branch =
                tail_call
                  ~st
                  ~pc
                  ~instrs:
                    [ Let (k', Prim (Extern "caml_resume_stack", [ Pv stack; tail; Pv k ])) ]
                  ~exact:true
                  ~in_cps:false
                  ~check:false
                  ~f:k'
                  [ f; arg ]
              in
              List.rev accu @ body, branch)
      | (Let (x, Prim (Extern "%perform", [ Pv _effect_ ])) as i) :: r -> (
          let perform_effect ~effect_ continuation_and_tail =
            let k = Var.fresh_n "k" in
            let body, branch =
              let f = Var.fresh_n "perform" in
              let prim =
                match continuation_and_tail with
                | None -> Prim (Extern "caml_perform_effect", [ Pv effect_; Pv k ])
                | Some (continuation, tail) ->
                    Prim
                      ( Extern "caml_reperform_effect"
                      , [ Pv effect_; continuation; tail; Pv k ] )
              in
              [ Let (f, prim) ], Return f
            in
            List.rev accu @ body, branch
          in
          match Var.Set.mem x st.cps_needed with
          | false -> rewrite (i :: accu) r
          | true -> (
              match i with
              | Let (_, Prim (Extern "%perform", [ Pv effect_ ])) ->
                  perform_effect ~effect_ None
              | Let (_, Prim (Extern "%reperform", [ Pv effect_; continuation; tail ])) ->
                  perform_effect ~effect_ (Some (continuation, tail))
              | _ -> assert false))
      | i :: r -> rewrite (rewrite_instr ~st i :: accu) r
    in
    rewrite [] block.body
  in
  let params =
    match Addr.Set.mem pc st.blocks_to_transform with
    | false -> block.params
    | true -> (
        match Addr.Hashtbl.find_opt st.is_continuation pc with
        | None -> (
            match Addr.Hashtbl.find_opt st.closure_info pc with
            | None -> block.params @ [ Var.fresh_n "k" ]
            | Some (params, (_, k_args)) ->
                let k = List.last k_args |> Option.get in
                params @ [ k ])
        | Some k -> block.params @ [ k ])
  in
  { params; body; branch = last }

let cps_transform ~live_vars:_ ~flow_info:_ ~cps_needed (p : program) =
  let blocks_to_transform, is_continuation = mark_continuations ~cps_needed p in
  let closure_info = closure_info ~cps_needed p in
  let matching_exn_handler = matching_exn_handler p in
  let in_cps = ref Var.Set.empty in
  let st =
    { in_cps
    ; cps_needed
    ; blocks_to_transform
    ; closure_info
    ; is_continuation
    ; matching_exn_handler
    }
  in
  let blocks = Addr.Map.mapi (fun pc block -> rewrite_instrs ~st (pc, block)) p.blocks in
  { p with blocks }, Var.Set.empty (* trampolined_calls *), !in_cps

(****)

let rewrite_instr_direct (instr : instr) : instr list =
  match instr with
  | Let (x, Prim (Extern "%perform", [ effect_ ])) ->
      (* In direct-style code, we just raise [Effect.Unhandled]. *)
      [ Let (x, Prim (Extern "caml_raise_unhandled", [ effect_ ])) ]
  | Let (x, Prim (Extern "%reperform", [ effect_; _continuation; _tail ])) ->
      (* In direct-style code, we just raise [Effect.Unhandled]. *)
      [ Let (x, Prim (Extern "caml_raise_unhandled", [ effect_ ])) ]
  | Let (x, Prim (Extern "%resume", [ stack; f; arg; tail ])) ->
      [ Let (x, Prim (Extern "caml_resume", [ f; arg; stack; tail ])) ]
  | _ -> [ instr ]

let rewrite_instrs_direct block =
  let body = List.concat_map block.body ~f:rewrite_instr_direct in
  { block with body }

let _rewrite_direct (p : program) =
  let blocks = Addr.Map.map rewrite_instrs_direct p.blocks in
  { p with blocks }

(****)

let split_blocks ~cps_needed (p : Code.program) =
  (* Ensure that function applications and effect primitives are in
     tail position *)
  let split_block pc block p =
    let is_split_point i r branch =
      match i with
      | Let (x, e) when effect_primitive_or_application e ->
          ((not (empty_body r))
          ||
          match branch with
          | Branch _ -> false
          | Return x' -> not (Var.equal x x')
          | _ -> true)
          && Var.Set.mem x cps_needed
      | _ -> false
    in
    let rec split (p : Code.program) pc block accu l branch =
      match l with
      | [] ->
          let block = { block with body = List.rev accu } in
          { p with blocks = Addr.Map.add pc block p.blocks }
      | (Let (x, e) as i) :: r when is_split_point i r branch ->
          let pc' = p.free_pc in
          let block' = { params = []; body = []; branch = block.branch } in
          let block =
            { block with body = List.rev (Let (x, e) :: accu); branch = Branch (pc', []) }
          in
          let p = { p with blocks = Addr.Map.add pc block p.blocks; free_pc = pc' + 1 } in
          split p pc' block' [] r branch
      | i :: r -> split p pc block (i :: accu) r branch
    in
    let rec should_split l branch =
      match l with
      | [] -> false
      | i :: r -> is_split_point i r branch || should_split r branch
    in
    if should_split block.body block.branch
    then split p pc block [] block.body block.branch
    else p
  in
  Addr.Map.fold split_block p.blocks p

(****)

(****)

let f ~flow_info ~live_vars p =
  Code.invariant p;
  if Poly.equal (Config.effects ()) `Fibers
  then p, Var.Set.empty, Var.Set.empty
  else (
    let t = Timer.make () in
    let cps_needed = Partial_cps_analysis.f p flow_info in
    let p, cps_needed =
      if double_translate ()
      then (
        let p, liftings = Lambda_lifting_simple.f ~to_lift:cps_needed p in
        let cps_needed =
          Var.Set.map
            (fun f -> try Subst.from_map liftings f with Not_found -> f)
            cps_needed
        in
        if debug ()
        then (
          let annot _ (i : Code.Print.xinstr) =
            match i with
            | Instr (Let (x, _)) when Var.Set.mem x cps_needed -> "CPS"
            | Instr _ | Last _ -> ""
          in
          Format.eprintf "@[<v>After lambda lifting:@,";
          Code.Print.program Format.err_formatter annot p;
          Format.eprintf "@]");
        p, cps_needed)
      else
        let p, cps_needed = rewrite_toplevel ~cps_needed p in
        p, cps_needed
    in
    let p = split_blocks ~cps_needed p in
    let p, trampolined_calls, in_cps = cps_transform ~live_vars ~flow_info ~cps_needed p in
    if Debug.find "times" () then Format.eprintf "  effects: %a@." Timer.print t;
    Code.invariant p;
    if debug ()
    then (
      Format.eprintf "@[<v>After CPS transform:@,";
      Code.Print.program Format.err_formatter (fun _ _ -> "") p;
      Format.eprintf "@]");
    p, trampolined_calls, in_cps)

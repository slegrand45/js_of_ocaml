(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
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

(* Beware automatic semi-colon insertion...
   {v
     a=b
     ++c
   v}
   is not the same as
   {v
     a=b ++c
   v}

   see so-called "restricted productions":
   the space cannot be replaced by a newline in the following expressions:
   {v
     e ++
     e --
     continue e
     break e
     return e
     throw
   v}
*)

(*
Source maps
===========
Most of this information was obtained by running the Firefox and
Chrome debuggers on some test programs.

The location of a declaration is determined by the first character of
the expression.

    var x = e
            ^

The location of other statements is determined by looking at the first
character of the statement.

    return e
    ^

Chrome will also stop at the very character after a return statement
before returning (which can be ambigous).

    return e;if ...
             ^

The location of the end of the function is determined by the closing brace.
Firefox will always stop their. Chrome only if there is no return statement.

    function f() { ... }
                       ^

For an arrow function Firefox stops on the last character, while
Chrome stops on the character right after.

    (x)=>x+1
           ^^

In Chrome the location of a function call is at the start of the name
of the function when it is explicit.

    f(e)         Math.cos(1.)
    ^                 ^

Otherwise, the location of the opening parenthesis is used. Firefox
always uses this location.

    (0,f)(e)(e')
         ^  ^

Usually, Chrome stops at the beginning of statements.

   if (e) { ... }
   ^

Firefox will rather stop on the expression when there is one.

   if (e) { ... }
       ^

The debugger don't stop at some statements, such as function
declarations, labelled statements, and block statements.

Chrome uses the name associated to the location of each bound variable
to determine its name [1].

   function f(x) { var y = ... }
            ^ ^        ^

Chrome uses the location of the opening parenthesis of a function
declaration to determine the function name in the stack [2].


    function f() { ... }
              ^

[1] https://github.com/ChromeDevTools/devtools-frontend/blob/11db398f811784395a6706cf3f800014d98171d9/front_end/models/source_map_scopes/NamesResolver.ts#L238-L243

[2] https://github.com/ChromeDevTools/devtools-frontend/blob/11db398f811784395a6706cf3f800014d98171d9/front_end/models/source_map_scopes/NamesResolver.ts#L765-L768
*)

open! Stdlib

let stats = Debug.find "output"

open Javascript

module StringSet = Set.Make (String)

(* let has_set_v set v = StringSet.mem v set *)
module PP = Pretty_print

module Make (D : sig
  val push_mapping : Pretty_print.pos -> Source_map.map -> unit

  val get_file_index : string -> int

  val get_name_index : string -> int
  [@@ocaml.warning "-32"]

  val hidden_location : Source_map.map

  val source_map_enabled : bool

  val accept_unnamed_var : bool
end) =
struct
  open D

  let name_of_label = function
    | Javascript.Label.L v ->
        assert accept_unnamed_var;
        Utf8_string.of_string_exn (Format.asprintf "<%a>" Code.Var.print v)
    | Javascript.Label.S n -> n

  let debug_enabled = Config.Flag.debuginfo ()

  let current_loc = ref U

  let last_mapping_has_a_name = ref false

  let output_debug_info f loc =
    let loc =
      (* We force a new mapping after an identifier, to avoid its name
         to bleed over other identifiers, using the current location
         when none is provided. *)
      match loc with
      | N when !last_mapping_has_a_name -> !current_loc
      | _ -> loc
    in
    match loc with
    | N -> ()
    | _ ->
        let location_changed = not (location_equal loc !current_loc) in
        (if source_map_enabled && (!last_mapping_has_a_name || location_changed)
         then
           match loc with
           | N | U | Pi { Parse_info.src = None | Some ""; _ } ->
               push_mapping (PP.pos f) hidden_location
           | Pi { Parse_info.src = Some file; line; col; _ } ->
               push_mapping
                 (PP.pos f)
                 (Source_map.Gen_Ori
                    { gen_line = -1
                    ; gen_col = -1
                    ; ori_source = get_file_index file
                    ; ori_line = line
                    ; ori_col = col
                    }));
        (if debug_enabled && location_changed
         then
           match loc with
           | N | U ->
               PP.non_breaking_space f;
               PP.string f "/*<<?>>*/";
               PP.non_breaking_space f
           | Pi pi ->
               PP.non_breaking_space f;
               PP.string f (Format.sprintf "/*<<%s>>*/" (Parse_info.to_string pi));
               PP.non_breaking_space f);
        current_loc := loc;
        last_mapping_has_a_name := false

  let sanitize_php v =
    StringLabels.map ~f:(fun c -> if Char.equal c '$' then '_' else c) v

    let php_label_counter = ref 0
  let unique_php_label l = incr php_label_counter; l ^ "_" ^ string_of_int !php_label_counter
  let php_label_map = ref []
  let get_php_label l prefix =
    match List.find_map ~f:(fun ((l', p'), res) -> if String.equal l l' && String.equal prefix p' then Some res else None) !php_label_map with
    | Some unique_l -> unique_l
    | None -> let unique_l = unique_php_label (prefix ^ "_" ^ l) in php_label_map := ((l, prefix), unique_l) :: !php_label_map; unique_l

  let identName f x =
    let x = if Config.Flag.php_output () then sanitize_php x else x in
    match Js_token.is_reserved x with
    | None -> PP.string f x
    | Some _ ->
        (* identName, that are reserved keyword, are escaped *)
        let rest = String.sub x ~pos:1 ~len:(String.length x - 1) in
        PP.string f (Printf.sprintf "\\u{%x}%s" (Char.code x.[0]) rest)

  let name_of_ident ident =
    let n = match ident with
      | S { name = Utf8 n; var; _ } ->
          (match var with
          | Some v when Config.Flag.php_output () && not (StringLabels.starts_with ~prefix:"caml_" n) ->
              n ^ "_" ^ string_of_int (Code.Var.idx v)
          | _ -> n)
      | V v -> "v" ^ string_of_int (Code.Var.idx v)
    in
    if Config.Flag.php_output () then sanitize_php n else n

  let ident ?(prefix = true) f ~kind ident =
    match ident with
    | S _ ->
        let n_str = name_of_ident ident in
        let is_param = match kind with `Binding -> true | _ -> false in
        if Config.Flag.php_output () && prefix &&
           (is_param
           || not (String.equal n_str "undefined"))
        then
          if not is_param && StringLabels.starts_with ~prefix:"caml_" n_str
          then PP.string f ("$GLOBALS['" ^ n_str ^ "']")
          else (
            (* Sanitize $ in identifier names for PHP output *)
            let sanitized =
              StringLabels.map ~f:(fun c -> if Char.equal c '$' then '_' else c) n_str
            in
            PP.string f ("$" ^ sanitized)
          )
        else if Config.Flag.php_output () && String.equal n_str "undefined" then PP.string f "null"
        else PP.string f n_str
    | V _ ->
        assert accept_unnamed_var;
        let n_str = name_of_ident ident in
        if Config.Flag.php_output () && prefix
        then PP.string f ("$" ^ n_str)
        else PP.string f ("<" ^ n_str ^ ">")

  let opt_identifier f ~kind i =
    match i with
    | None -> ()
    | Some i ->
        PP.space f;
        ident f ~kind i

  let early_error _ = assert false

  type prec =
    | Expression (* 0  *)
    | AssignementExpression (* 1  *)
    | ConditionalExpression (* 2  *)
    | ShortCircuitExpression
    | CoalesceExpression
    | LogicalORExpression (* 3  *)
    | LogicalANDExpression (* 4  *)
    | BitwiseORExpression (* 5  *)
    | BitwiseXORExpression (* 6  *)
    | BitwiseANDExpression (* 7  *)
    | EqualityExpression (* 8  *)
    | RelationalExpression (* 9  *)
    | ShiftExpression (* 10 *)
    | AdditiveExpression (* 11 *)
    | MultiplicativeExpression (* 12 *)
    | ExponentiationExpression
    | UnaryExpression (* 13 *)
    | UpdateExpression (* 14 *)
    | LeftHandSideExpression (* 15 *)
    | NewExpression
    | CallOrMemberExpression
    | MemberExpression (* 16 *)

  module Prec = struct
    let compare (a : prec) (b : prec) = Poly.compare a b

    [@@@ocaml.warning "-32"]

    let ( <= ) a b = compare a b <= 0

    let ( >= ) a b = compare a b >= 0

    let ( < ) a b = compare a b < 0

    let ( > ) a b = compare a b > 0

    let ( = ) a b = compare a b = 0
  end

  let op_prec op =
    match op with
    | Eq
    | StarEq
    | SlashEq
    | ModEq
    | PlusEq
    | DotEq
    | MinusEq
    | LslEq
    | AsrEq
    | LsrEq
    | BandEq
    | BxorEq
    | BorEq
    | OrEq
    | AndEq
    | ExpEq
    | CoalesceEq -> AssignementExpression, LeftHandSideExpression, AssignementExpression
    | Coalesce -> CoalesceExpression, BitwiseORExpression, BitwiseORExpression
    | Or -> LogicalORExpression, LogicalORExpression, LogicalORExpression
    | And -> LogicalANDExpression, LogicalANDExpression, LogicalANDExpression
    | Bor -> BitwiseORExpression, BitwiseORExpression, BitwiseORExpression
    | Bxor -> BitwiseXORExpression, BitwiseXORExpression, BitwiseXORExpression
    | Band -> BitwiseANDExpression, BitwiseANDExpression, BitwiseANDExpression
    | EqEq | NotEq | EqEqEq | NotEqEq ->
        EqualityExpression, EqualityExpression, RelationalExpression
    | Gt | GtInt | Ge | GeInt | Lt | LtInt | Le | LeInt | InstanceOf | In ->
        RelationalExpression, RelationalExpression, ShiftExpression
    | Lsl | Lsr | Asr -> ShiftExpression, ShiftExpression, AdditiveExpression
    | Plus | Dot | Minus -> AdditiveExpression, AdditiveExpression, MultiplicativeExpression
    | Mul | Div | Mod ->
        MultiplicativeExpression, MultiplicativeExpression, ExponentiationExpression
    | Exp -> ExponentiationExpression, UpdateExpression, ExponentiationExpression

  let op_str op =
    match op with
    | Eq -> "="
    | StarEq -> "*="
    | SlashEq -> "/="
    | ModEq -> "%="
    | PlusEq -> "+="
    | DotEq -> ".="
    | MinusEq -> "-="
    | Or -> "||"
    | OrEq -> "||="
    | And -> "&&"
    | AndEq -> "&&="
    | Bor -> "|"
    | Bxor -> "^"
    | Band -> "&"
    | EqEq -> "=="
    | NotEq -> "!="
    | EqEqEq -> "==="
    | NotEqEq -> "!=="
    | LslEq -> "<<="
    | AsrEq -> ">>="
    | LsrEq -> ">>>="
    | BandEq -> "&="
    | BxorEq -> "^="
    | BorEq -> "|="
    | Lt | LtInt -> "<"
    | Le | LeInt -> "<="
    | Gt | GtInt -> ">"
    | Ge | GeInt -> ">="
    | Lsl -> "<<"
    | Lsr -> ">>>"
    | Asr -> ">>"
    | Plus -> "+"
    | Dot -> if Config.Flag.php_output () then "." else "+"
    | Minus -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Mod -> "%"
    | Exp -> "**"
    | ExpEq -> "**="
    | CoalesceEq -> "??="
    | Coalesce -> "??"
    | InstanceOf | In -> assert false

  let unop_str op =
    match op with
    | Not -> "!"
    | Neg -> "-"
    | Pl -> "+"
    | Bnot -> "~"
    | IncrA | IncrB | DecrA | DecrB | Typeof | Void | Delete | Await -> assert false

  let rec ends_with_if_without_else st =
    match fst st with
    | Labelled_statement (_, st)
    | If_statement (_, _, Some st)
    | While_statement (_, st)
    | For_statement (_, _, _, st)
    | With_statement (_, st)
    | ForIn_statement (_, _, st) -> ends_with_if_without_else st
    | ForOf_statement (_, _, st) -> ends_with_if_without_else st
    | ForAwaitOf_statement (_, _, st) -> ends_with_if_without_else st
    | If_statement (_, _, None) -> true
    | Block _
    | Variable_statement _
    | Empty_statement
    | Expression_statement _
    | Continue_statement _
    | Break_statement _
    | Return_statement _
    | Throw_statement _
    | Do_while_statement _
    | Switch_statement _
    | Try_statement _
    | Function_declaration _
    | Class_declaration _
    | Debugger_statement
    | Import _
    | Export _ -> false

  let starts_with ~obj ~funct ~class_ ~let_identifier ~async_identifier l e =
    let rec traverse l e =
      match e with
      | EObj _ -> obj
      | EFun _ -> funct
      | EClass _ -> class_
      | EVar (S { name = Utf8 "let"; _ }) -> let_identifier
      | EVar (S { name = Utf8 "async"; _ }) -> async_identifier
      | ESeq (e, _) -> Prec.(l <= Expression) && traverse Expression e
      | ECond (e, _, _) ->
          Prec.(l <= ConditionalExpression) && traverse ShortCircuitExpression e
      | EAssignTarget (ObjectTarget _) -> obj
      | EAssignTarget (ArrayTarget _) -> false
      | EBin (op, e, _) ->
          let out, lft, _rght = op_prec op in
          Prec.(l <= out) && traverse lft e
      | EUn ((IncrA | DecrA), e) ->
          Prec.(l <= UpdateExpression) && traverse LeftHandSideExpression e
      | ECallTemplate (EFun _, _, _) ->
          (* We force parens around the function in that case.*)
          false
      | ECallTemplate (e, _, _)
      | ECall (e, _, _, _)
      | EAccess (e, _, _)
      | EDot (e, _, _)
      | EDotPrivate (e, _, _) -> traverse CallOrMemberExpression e
      | EArrow _
      | EVar _
      | EStr _
      | ETemplate _
      | EArr _
      | EBool _
      | ENum _
      | ERegexp _
      | EUn _
      | ENew _
      | EYield _
      | EPrivName _ -> false
      | CoverCallExpressionAndAsyncArrowHead e
      | CoverParenthesizedExpressionAndArrowParameterList e -> early_error e
    in
    traverse l e

  let contains ~in_ l e =
    let rec traverse l e =
      match e with
      | EObj _ -> false
      | EFun _ -> false
      | EClass _ -> false
      | EVar (S { name = Utf8 "in"; _ }) -> true
      | ESeq (e1, e2) ->
          Prec.(l <= Expression) && (traverse Expression e1 || traverse Expression e2)
      | ECond (e1, e2, e3) ->
          Prec.(l <= ConditionalExpression)
          && (traverse ShortCircuitExpression e1
             || traverse ShortCircuitExpression e2
             || traverse ShortCircuitExpression e3)
      | EAssignTarget (ObjectTarget _) -> false
      | EAssignTarget (ArrayTarget _) -> false
      | EBin (op, e1, e2) ->
          let out, lft, rght = op_prec op in
          Prec.(l <= out)
          && ((match op with
                | In -> in_
                | _ -> false)
             || traverse lft e1
             || traverse rght e2)
      | EUn ((IncrA | DecrA | IncrB | DecrB), e) ->
          Prec.(l <= UpdateExpression) && traverse LeftHandSideExpression e
      | EUn (_, e) -> Prec.(l <= UnaryExpression) && traverse UnaryExpression e
      | ECallTemplate (EFun _, _, _) ->
          (* We force parens around the function in that case.*)
          false
      | ECallTemplate (e, _, _)
      | ECall (e, _, _, _)
      | EAccess (e, _, _)
      | EDot (e, _, _)
      | EDotPrivate (e, _, _) -> traverse CallOrMemberExpression e
      | EArrow _
      | EVar _
      | EStr _
      | ETemplate _
      | EArr _
      | EBool _
      | ENum _
      | ERegexp _
      | ENew _
      | EYield _
      | EPrivName _ -> false
      | CoverCallExpressionAndAsyncArrowHead e
      | CoverParenthesizedExpressionAndArrowParameterList e -> early_error e
    in
    traverse l e

  (* The debuggers do not stop on some statements, like function
     declarations. So there is no point in outputting some debug
     information there. *)
  let stop_on_statement st =
    match st with
    | Block _
    | Variable_statement _
    | Function_declaration _
    | Class_declaration _
    | Empty_statement
    | Labelled_statement _
    | Import _
    | Export _ -> false
    | Expression_statement _
    | If_statement _
    | Do_while_statement _
    | While_statement _
    | For_statement _
    | ForIn_statement _
    | ForOf_statement _
    | ForAwaitOf_statement _
    | Continue_statement _
    | Break_statement _
    | Return_statement _
    | With_statement _
    | Switch_statement _
    | Throw_statement _
    | Try_statement _
    | Debugger_statement -> true

  let best_string_quote s =
    let simple = ref 0 and double = ref 0 in
    for i = 0 to String.length s - 1 do
      match s.[i] with
      | '\'' -> incr simple
      | '"' -> incr double
      | _ -> ()
    done;
    if !simple < !double then '\'' else '"'

  let pp_string f ?(quote = '"') s =
    let l = String.length s in
    let b = Buffer.create (String.length s + 2) in
    Buffer.add_char b quote;
    for i = 0 to l - 1 do
      let c = s.[i] in
      match c with
      | '\000' when i = l - 1 || not (Char.is_digit s.[i + 1]) ->
          Buffer.add_string b "\\0"
      | '\b' (* 008 *) -> Buffer.add_string b "\\b"
      | '\t' (* 009 *) -> Buffer.add_string b "\\t"
      | '\n' (* 010 *) -> Buffer.add_string b "\\n"
      | '\011' -> Buffer.add_string b "\\v"
      | '\012' -> Buffer.add_string b "\\f"
      | '\r' (* 013 *) -> Buffer.add_string b "\\r"
      (* https://github.com/ocsigen/js_of_ocaml/issues/898 *)
      | '/' when i > 0 && Char.equal s.[i - 1] '<' -> Buffer.add_string b "\\/"
      | '\000' .. '\031' | '\127' ->
          Buffer.add_string b "\\x";
          Buffer.add_char_hex b c
      | _ ->
          if Char.equal c quote
          then (
            Buffer.add_char b '\\';
            Buffer.add_char b c)
          else Buffer.add_char b c
    done;
    Buffer.add_char b quote;
    PP.string f (Buffer.contents b)

  let pp_string_lit f (Stdlib.Utf8_string.Utf8 s) =
    let quote = best_string_quote s in
    pp_string f ~quote s

  let pp_ident_or_string_lit f (Stdlib.Utf8_string.Utf8 s_lit as s) =
    if is_ident s_lit then PP.string f s_lit else pp_string_lit f s

  let rec comma_list f ~force_last_comma f_elt l =
    match l with
    | [] -> ()
    | [ x ] ->
        PP.start_group f 0;
        f_elt f x;
        if force_last_comma x then PP.string f ",";
        PP.end_group f
    | x :: r ->
        PP.start_group f 0;
        f_elt f x;
        PP.end_group f;
        PP.string f ",";
        PP.space f;
        comma_list f ~force_last_comma f_elt r

  let comma_list_rest f ~force_last_comma f_elt l f_rest rest =
    match l, rest with
    | [], None -> ()
    | [], Some rest ->
        PP.start_group f 0;
        PP.string f "...";
        f_rest f rest;
        PP.end_group f
    | l, None -> comma_list f ~force_last_comma f_elt l
    | l, Some r ->
        comma_list f ~force_last_comma:(fun _ -> false) f_elt l;
        PP.string f ",";
        PP.space f;
        PP.start_group f 0;
        PP.string f "...";
        f_rest f r;
        PP.end_group f

  let rec expression (l : prec) f e =
    match e with
    | EVar v -> ident f ~kind:`Reference v
    | ESeq (e1, e2) ->
        if Config.Flag.php_output ()
        then (
          (* In PHP, sequences need to use caml_seq helper since PHP doesn't have comma operator *)
          if Prec.(l > Expression)
          then (
            PP.start_group f 1;
            PP.string f "(");
          PP.string f "caml_seq(";
          expression Expression f e1;
          PP.string f ", ";
          expression Expression f e2;
          PP.string f ")";
          if Prec.(l > Expression)
          then (
            PP.string f ")";
            PP.end_group f))
        else (
          if Prec.(l > Expression)
          then (
            PP.start_group f 1;
            PP.string f "(");
          expression Expression f e1;
          PP.string f ",";
          PP.space f;
          expression Expression f e2;
          if Prec.(l > Expression)
          then (
            PP.string f ")";
            PP.end_group f))
    | EFun (i, decl) -> function_declaration' f i decl
    | EClass (i, cl_decl) -> class_declaration f i cl_decl
    | EArrow ((k, p, b, pc), consise, _arrow_info) ->
        if Prec.(l > AssignementExpression)
        then (
          PP.start_group f 1;
          PP.string f "(");
        if Config.Flag.php_output ()
        then (
          (* PHP does not support arrow functions with =>, use anonymous functions instead *)
          let old_php_label_map = !php_label_map in
          php_label_map := [];
          PP.start_group f 1;
          PP.start_group f 0;
          (match k with
          | { async = true; generator = false } ->
              PP.string f "async function";
              PP.non_breaking_space f
          | { async = false; generator = false } ->
              PP.string f "function";
              PP.non_breaking_space f
          | { async = true | false; generator = true } -> assert false);
          PP.string f "(";
          formal_parameter_list f p;
          PP.string f ")";
          (* Calculate and output use clause *)
          (* Collect uses from function body *)
          let body_uses = collect_uses_in_statement_list [] b in
          (* Collect defs from function body *)
          let body_defs = collect_defs_in_statement_list [] b in
          let param_names = get_param_names p in
          (* Free vars = Uses \ (Defs U Params U Globals) *)
          let all_defs = List.concat [ body_defs; param_names ] in
          let free_vars =
            List.filter body_uses ~f:(fun v ->
                not (List.mem ~eq:String.equal v all_defs)
                && not (String.equal v "globalThis")
                && not (String.equal v "undefined")
                && not (String.equal v "NaN")
                && not (String.equal v "Infinity")
                && not (StringLabels.starts_with ~prefix:"caml_" v))
          in
          if not (List.is_empty free_vars)
          then (
            PP.string f " use (";
            let vars = List.sort_uniq ~cmp:String.compare free_vars in
            let rec output_vars = function
              | [] -> ()
              | [ v ] ->
                  PP.string f "&$";
                  PP.string f (sanitize_php v)
              | v :: rest ->
                  PP.string f "&$";
                  PP.string f (sanitize_php v);
                  PP.string f ", ";
                  output_vars rest
            in
            output_vars vars;
            PP.string f ")");
          PP.end_group f;
          (match b, consise with
          | [ (Return_statement (Some e, _loc'), loc'') ], true ->
              PP.string f " {";
              PP.break f;
              output_debug_info f loc'';
              PP.string f "return ";
              parenthesized_expression ~obj:true AssignementExpression f e;
              PP.string f ";";
              output_debug_info f pc;
              PP.string f "}";
          | l, _ ->
              let b =
                match l with
                | [ (Block l, _) ] -> l
                | l -> l
              in
              PP.string f " {";
              PP.break f;
              function_body f b;
              output_debug_info f pc;
              PP.string f "}");
          php_label_map := old_php_label_map;
          PP.end_group f)
        else (
          (* JavaScript arrow function output *)
          PP.start_group f 1;
          PP.start_group f 0;
          (match k with
          | { async = true; generator = false } ->
              PP.string f "async";
              PP.non_breaking_space f
          | { async = false; generator = false } -> ()
          | { async = true | false; generator = true } -> assert false);
          (match p with
          | { list = [ ((BindingIdent _, None) as x) ]; rest = None } ->
              formal_parameter f x;
              PP.string f "=>"
          | _ ->
              PP.start_group f 1;
              PP.string f "(";
              formal_parameter_list f p;
              PP.string f ")=>";
              PP.end_group f);
          PP.end_group f;
          (match b, consise with
          | [ (Return_statement (Some e, _loc'), _loc'') ], true ->
              (* Should not starts with '{' *)
              PP.start_group f 1;
              PP.break1 f;
              parenthesized_expression ~obj:true AssignementExpression f e;
              PP.end_group f;
              output_debug_info f pc
          | l, _ ->
              let b =
                match l with
                | [ (Block l, _) ] -> l
                | l -> l
              in
              PP.string f "{";
              PP.break f;
              function_body f b;
              output_debug_info f pc;
              PP.string f "}");
          PP.end_group f);
        if Prec.(l > AssignementExpression)
        then (
          PP.string f ")";
          PP.end_group f)
    | ECall (e, access_kind, el, loc) ->
        (* Need parentheses also if within an expression [new e] *)
        if Prec.(l = NewExpression || l > CallOrMemberExpression)
        then (
          PP.start_group f 1;
          PP.string f "(");
        output_debug_info f loc;
        PP.start_group f 1;
        expression CallOrMemberExpression f e;
        PP.break f;
        (* Make sure that the opening parenthesis has the appropriate info *)
        output_debug_info f loc;
        PP.start_group f 1;
        (match access_kind with
        | ANormal -> PP.string f "("
        | ANullish -> PP.string f "?.(");
        arguments f el;
        PP.string f ")";
        PP.end_group f;
        PP.end_group f;
        if Prec.(l = NewExpression || l > CallOrMemberExpression)
        then (
          PP.string f ")";
          PP.end_group f)
    | ECallTemplate (e, t, loc) ->
        (* Need parentheses also if within an expression [new e] *)
        if Prec.(l = NewExpression || l > CallOrMemberExpression)
        then (
          PP.start_group f 1;
          PP.string f "(");
        output_debug_info f loc;
        PP.start_group f 1;
        parenthesized_expression ~funct:true CallOrMemberExpression f e;
        PP.break f;
        PP.start_group f 1;
        template f t;
        PP.end_group f;
        PP.end_group f;
        if Prec.(l = NewExpression || l > CallOrMemberExpression)
        then (
          PP.string f ")";
          PP.end_group f)
    | EStr x -> pp_string_lit f x
    | ETemplate l -> template f l
    | EBool b -> PP.string f (if b then "true" else "false")
    | ENum num ->
        let s = Num.to_string num in
        let need_parent =
          if Num.is_neg num
          then
            Prec.(l > UnaryExpression)
            (* Negative numbers may need to be parenthesized. *)
          else
            Prec.(l >= CallOrMemberExpression)
            (* Parenthesize as well when followed by a dot. *)
            && (not (Char.equal s.[0] 'I'))
            (* Infinity *)
            && not (Char.equal s.[0] 'N')
          (* NaN *)
        in
        if need_parent then PP.string f "(";
        PP.string f s;
        if need_parent then PP.string f ")"
    | EUn (((Typeof | Void | Delete | Await) as op), e) ->
        let p = UnaryExpression in
        if Prec.(l > p)
        then (
          PP.start_group f 1;
          PP.string f "(");
        PP.start_group f 0;
        if Config.Flag.php_output ()
        then
          match op with
          | Typeof ->
              PP.string f "caml_typeof(";
              expression p f e;
              PP.string f ")"
          | Void ->
              (* void e in PHP is just null *)
              PP.string f "null"
          | Delete ->
              (* Fallback for delete - should not happen in normal OCaml code *)
              PP.string f "delete";
              PP.space f;
              expression p f e
          | Await ->
              (* Fallback for await - should not happen in normal OCaml code *)
              PP.string f "await";
              PP.space f;
              expression p f e
          | _ -> assert false
        else (
          let name =
            match op with
            | Typeof -> "typeof"
            | Void -> "void"
            | Delete -> "delete"
            | Await -> "await"
            | _ -> assert false
          in
          PP.string f name;
          PP.space f;
          expression p f e);
        PP.end_group f;
        if Prec.(l > p)
        then (
          PP.string f ")";
          PP.end_group f)
    | EUn (((IncrB | DecrB) as op), e) ->
        let p = UpdateExpression in
        if Prec.(l > p)
        then (
          PP.start_group f 1;
          PP.string f "(");
        (match op with
        | IncrB -> PP.string f "++"
        | DecrB -> PP.string f "--"
        | _ -> assert false);
        expression UnaryExpression f e;
        if Prec.(l > p)
        then (
          PP.string f ")";
          PP.end_group f)
    | EUn (((IncrA | DecrA) as op), e) ->
        let p = UpdateExpression in
        if Prec.(l > p)
        then (
          PP.start_group f 1;
          PP.string f "(");
        expression LeftHandSideExpression f e;
        (match op with
        | IncrA -> PP.string f "++"
        | DecrA -> PP.string f "--"
        | _ -> assert false);
        if Prec.(l > p)
        then (
          PP.string f ")";
          PP.end_group f)
    | EUn (op, e) ->
        let p = UnaryExpression in
        let need_parent = Prec.(l > p) in
        if need_parent
        then (
          PP.start_group f 1;
          PP.string f "(");
        PP.string f (unop_str op);
        PP.space f;
        expression p f e;
        if need_parent
        then (
          PP.string f ")";
          PP.end_group f)
    | EBin (((InstanceOf | In) as op), e1, e2) ->
        let out, lft, rght = op_prec InstanceOf in
        if Prec.(l > out)
        then (
          PP.start_group f 1;
          PP.string f "(");
        PP.start_group f 0;
        expression lft f e1;
        PP.space f;
        (match op with
        | InstanceOf when Config.Flag.php_output () ->
            (* In PHP, use instanceof with proper class name handling *)
            PP.string f "instanceof ";
            expression rght f e2
        | _ ->
            let name =
              match op with
              | InstanceOf -> "instanceof"
              | In -> "in"
              | _ -> assert false
            in
            PP.string f name;
            PP.space f;
            expression rght f e2);
        PP.end_group f;
        if Prec.(l > out)
        then (
          PP.string f ")";
          PP.end_group f)
    | EBin
        ( (( Eq
           | StarEq
           | SlashEq
           | ModEq
           | PlusEq
           | MinusEq
           | LslEq
           | AsrEq
           | BandEq
           | BxorEq
           | BorEq
           | OrEq
           | AndEq
           | ExpEq
           | CoalesceEq ) as op)
        , e1
        , e2 ) ->
        let out, lft, rght = op_prec op in
        let lft =
          (* We can have e sequence of coalesce: e1 ?? e2 ?? e3,
             but each expressions should be a BitwiseORExpression *)
          match e1, op with
          | EBin (Coalesce, _, _), Coalesce -> CoalesceExpression
          | _ -> lft
        in
        PP.start_group f 0;
        if Prec.(l > out) then PP.string f "(";
        PP.start_group f 0;
        expression lft f e1;
        PP.space f;
        PP.string f (op_str op);
        PP.end_group f;
        PP.start_group f 1;
        PP.space f;
        expression rght f e2;
        PP.end_group f;
        if Prec.(l > out) then PP.string f ")";
        PP.end_group f
    | EBin (LsrEq, e1, e2) ->
        (* Special handling for LsrEq (>>>=) in PHP mode *)
        let out, lft, rght = op_prec LsrEq in
        PP.start_group f 0;
        if Prec.(l > out) then PP.string f "(";
        expression lft f e1;
        if Config.Flag.php_output () then
          begin
            PP.string f " = caml_shift_right_unsigned(";
            expression lft f e1;
            PP.string f ", ";
            expression rght f e2;
            PP.string f ")";
          end
        else
          begin
            PP.space f;
            PP.string f ">>>=";
            PP.space f;
            expression rght f e2;
          end;
        if Prec.(l > out) then PP.string f ")";
        PP.end_group f
    | EBin (op, e1, e2) ->
        let out, lft, rght = op_prec op in
        let lft =
          (* We can have e sequence of coalesce: e1 ?? e2 ?? e3,
             but each expressions should be a BitwiseORExpression *)
          match e1, op with
          | EBin (Coalesce, _, _), Coalesce -> CoalesceExpression
          | _ -> lft
        in
        (* Special handling for Lsr (>>>) in PHP mode *)
        if Config.Flag.php_output () && Poly.equal op Lsr then
          begin
            PP.start_group f 0;
            if Prec.(l > out) then PP.string f "(";
            PP.string f "caml_shift_right_unsigned(";
            expression lft f e1;
            PP.string f ", ";
            expression rght f e2;
            PP.string f ")";
            if Prec.(l > out) then PP.string f ")";
            PP.end_group f
          end
        else
          begin
            PP.start_group f 0;
            if Prec.(l > out) then PP.string f "(";
            expression lft f e1;
            PP.space f;
            PP.start_group f 1;
            PP.string f (op_str op);
            PP.space f;
            expression rght f e2;
            if Prec.(l > out) then PP.string f ")";
            PP.end_group f;
            PP.end_group f
          end
    | EAssignTarget t -> (
        let property f p =
          match p with
          | TargetPropertyId (Prop_and_ident id, None) -> ident f ~kind:`Reference id
          | TargetPropertyId (Prop_and_ident id, Some (e, _)) ->
              ident f ~kind:`Reference id;
              PP.space f;
              PP.string f "=";
              PP.space f;
              expression AssignementExpression f e
          | TargetProperty (pn, e, None) ->
              PP.start_group f 0;
              property_name f pn;
              PP.string f ":";
              PP.space f;
              expression AssignementExpression f e;
              PP.end_group f
          | TargetProperty (pn, e, Some (ini, _)) ->
              PP.start_group f 0;
              property_name f pn;
              PP.string f ":";
              PP.space f;
              expression AssignementExpression f e;
              PP.space f;
              PP.string f "=";
              PP.space f;
              expression AssignementExpression f ini;
              PP.end_group f
          | TargetPropertySpread e ->
              PP.string f "...";
              expression AssignementExpression f e
          | TargetPropertyMethod (n, m) -> method_ f property_name n m
        in
        let element f p =
          match p with
          | TargetElementHole -> ()
          | TargetElementId (id, None) -> ident f ~kind:`Reference id
          | TargetElementId (id, Some (e, _)) ->
              ident f ~kind:`Reference id;
              PP.space f;
              PP.string f "=";
              PP.space f;
              expression AssignementExpression f e
          | TargetElement e -> expression AssignementExpression f e
          | TargetElementSpread e ->
              PP.string f "...";
              expression AssignementExpression f e
        in
        match t with
        | ObjectTarget list ->
            PP.start_group f 1;
            PP.string f "{";
            comma_list f ~force_last_comma:(fun _ -> false) property list;
            PP.string f "}";
            PP.end_group f
        | ArrayTarget list ->
            PP.start_group f 1;
            PP.string f "[";
            comma_list
              f
              ~force_last_comma:(function
                | TargetElementHole -> true
                | _ -> false)
              element
              list;
            PP.string f "]";
            PP.end_group f)
    | EArr el ->
        PP.start_group f 1;
        if Config.Flag.php_output () then PP.string f "new CamlBlock(";
        PP.string f "[";
        element_list f el;
        PP.string f "]";
        if Config.Flag.php_output () then PP.string f ")";
        PP.end_group f
    | EAccess (e, access_kind, e') ->
        PP.start_group f 1;
        let l' =
          match l with
          | NewExpression | MemberExpression -> MemberExpression
          | _ -> CallOrMemberExpression
        in
        if Config.Flag.php_output () then
          match e with
          | EVar (S { name = Utf8 "globalThis"; _ }) ->
              PP.string f "$GLOBALS[";
              expression Expression f e';
              PP.string f "]"
          | _ ->
              expression l' f e;
              PP.break f;
              PP.start_group f 1;
              (match access_kind with
              | ANormal -> PP.string f "["
              | ANullish -> PP.string f "?.[");
              expression Expression f e';
              PP.string f "]";
              PP.end_group f
        else (
          expression l' f e;
          PP.break f;
          PP.start_group f 1;
          (match access_kind with
          | ANormal -> PP.string f "["
          | ANullish -> PP.string f "?.[");
          expression Expression f e';
          PP.string f "]";
          PP.end_group f);
        PP.end_group f
    | EDot (e, access_kind, Utf8 nm) ->
        if Config.Flag.php_output () then
          if String.equal nm "length" then
            (* PHP doesn't support ->length on arrays, use caml_js_length helper *)
            (PP.string f "caml_js_length(";
             expression CallOrMemberExpression f e;
             PP.string f ")")
          else
            match e with
            | EVar (S { name = Utf8 "globalThis"; _ }) ->
                PP.string f "$GLOBALS['";
                PP.string f nm;
                PP.string f "']"
            | _ ->
                let l' =
                  match l with
                  | NewExpression | MemberExpression -> MemberExpression
                  | _ -> CallOrMemberExpression
                in
                expression l' f e;
                PP.string f "->";
                PP.string f nm
        else (
          let l' =
            match l with
            | NewExpression | MemberExpression -> MemberExpression
            | _ -> CallOrMemberExpression
          in
          expression l' f e;
          (match access_kind with
          | ANormal -> PP.string f "."
          | ANullish -> PP.string f "?.");
          PP.string f nm)
    | EDotPrivate (e, access_kind, Utf8 nm) ->
        let l' =
          match l with
          | NewExpression | MemberExpression -> MemberExpression
          | _ -> CallOrMemberExpression
        in
        expression l' f e;
        (match access_kind with
        | ANormal -> PP.string f "->"
        | ANullish -> PP.string f "?->");
        PP.string f nm
    | ENew (e, None, loc) ->
        if Prec.(l > NewExpression)
        then (
          PP.start_group f 1;
          PP.string f "(");
        PP.start_group f 1;
        output_debug_info f loc;
        PP.string f "new";
        PP.space f;
        expression NewExpression f e;
        PP.end_group f;
        if Prec.(l > NewExpression)
        then (
          PP.string f ")";
          PP.end_group f)
    | ENew (e, Some el, loc) ->
        PP.start_group f 1;
        output_debug_info f loc;
        PP.string f "new";
        PP.space f;
        expression MemberExpression f e;
        PP.break f;
        PP.start_group f 1;
        PP.string f "(";
        arguments f el;
        PP.string f ")";
        PP.end_group f;
        PP.end_group f
    | ECond (e, e1, e2) ->
        if Prec.(l > ConditionalExpression)
        then (
          PP.start_group f 1;
          PP.string f "(");
        PP.start_group f 1;
        PP.start_group f 0;
        expression ShortCircuitExpression f e;
        PP.end_group f;
        PP.space f;
        PP.start_group f 1;
        PP.start_group f 0;
        PP.string f "?";
        PP.space f;
        PP.end_group f;
        expression AssignementExpression f e1;
        PP.end_group f;
        PP.space f;
        PP.start_group f 1;
        PP.start_group f 0;
        PP.string f ":";
        PP.space f;
        PP.end_group f;
        (if Config.Flag.php_output ()
         then (
           (* PHP 8+ requires parentheses around nested ternaries in else branch *)
           PP.string f "(";
           expression AssignementExpression f e2;
           PP.string f ")")
         else expression AssignementExpression f e2);        PP.end_group f;
        PP.end_group f;
        if Prec.(l > ConditionalExpression)
        then (
          PP.string f ")";
          PP.end_group f)
    | EObj lst ->
        PP.start_group f 1;
        PP.string f "{";
        property_list f lst;
        PP.string f "}";
        PP.end_group f
    | ERegexp (s, opt) -> (
        PP.string f "/";
        PP.string f s;
        PP.string f "/";
        match opt with
        | None -> ()
        | Some o -> PP.string f o)
    | EYield { delegate; expr = e } ->
        let kw =
          match delegate with
          | false -> "yield"
          | true -> "yield*"
        in
        if Prec.(l > AssignementExpression)
        then (
          PP.start_group f 1;
          PP.string f "(");
        (match e with
        | None -> PP.string f kw
        | Some e ->
            PP.start_group f 7;
            PP.string f kw;
            PP.non_breaking_space f;
            PP.start_group f 0;
            expression AssignementExpression f e;
            PP.end_group f;
            PP.end_group f
            (* There MUST be a space between the yield and its
                 argument. A line return will not work *));
        if Prec.(l > AssignementExpression)
        then (
          PP.end_group f;
          PP.string f ")")
    | EPrivName (Utf8 i) ->
        PP.string f "#";
        PP.string f i
    | CoverCallExpressionAndAsyncArrowHead e
    | CoverParenthesizedExpressionAndArrowParameterList e -> early_error e

  and template f l =
    PP.string f "`";
    List.iter l ~f:(function
      | TStr (Utf8 s) -> PP.string f s
      | TExp e ->
          PP.string f "${";
          expression AssignementExpression f e;
          PP.string f "}");
    PP.string f "`"

  and property_name f n =
    match n with
    | PNI (Utf8 s) -> PP.string f s
    | PNS s -> pp_string_lit f s
    | PNN v -> expression Expression f (ENum v)
    | PComputed e ->
        PP.string f "[";
        expression Expression f e;
        PP.string f "]"

  and property_list f l = comma_list f ~force_last_comma:(fun _ -> false) property l

  and property f p =
    match p with
    | Property (pn, e) ->
        PP.start_group f 0;
        property_name f pn;
        PP.string f ":";
        PP.space f;
        expression AssignementExpression f e;
        PP.end_group f
    | PropertySpread e ->
        PP.string f "...";
        expression AssignementExpression f e
    | PropertyMethod (n, m) -> method_ f property_name n m
    | CoverInitializedName (e, _, _) -> early_error e

  and method_ : 'a. _ -> (PP.t -> 'a -> unit) -> 'a -> method_ -> unit =
   fun (type a) f (name : PP.t -> a -> unit) (n : a) (m : method_) ->
    match m with
    | MethodGet (k, l, b, loc') | MethodSet (k, l, b, loc') ->
        (match k with
        | { async = false; generator = false } -> ()
        | _ -> assert false);
        let prefix =
          match m with
          | MethodGet _ -> "get"
          | MethodSet _ -> "set"
          | _ -> assert false
        in
        function_declaration f prefix name (Some n) l b loc'
    | Method (k, l, b, loc') ->
        let fpn f () =
          (match k with
          | { async = false; generator = false } -> ()
          | { async = false; generator = true } ->
              PP.string f "*";
              PP.space f
          | { async = true; generator = false } ->
              PP.string f "async";
              PP.non_breaking_space f
          | { async = true; generator = true } ->
              PP.string f "async*";
              PP.space f);
          name f n
        in
        function_declaration f "" fpn (Some ()) l b loc'

  and element_list f el =
    comma_list
      f
      ~force_last_comma:(function
        | ElementHole -> true
        | _ -> false)
      element
      el

  and element f (e : element) =
    match e with
    | ElementHole ->
        (* In PHP, array holes must be filled with null to avoid fatal errors *)
        if Config.Flag.php_output () then PP.string f "null"
    | Element e ->
        PP.start_group f 0;
        expression AssignementExpression f e;
        PP.end_group f
    | ElementSpread e ->
        PP.start_group f 0;
        PP.string f "...";
        expression AssignementExpression f e;
        PP.end_group f

  and formal_parameter f e = binding_element f e

  and formal_parameter_list f { list; rest } =
    comma_list_rest
      f
      ~force_last_comma:(fun _ -> false)
      formal_parameter
      list
      binding
      rest

  and function_body f b = statement_list f ~skip_last_semi:true b

  (* Collect variables defined (bound) in a statement list *)
  and extract_name_from_binding = function
    | BindingIdent id -> Some (name_of_ident id)
    | BindingPattern _ -> None

  and get_param_names l =
    let names = List.fold_left l.list ~init:[] ~f:(fun acc (binding, _) ->
        match extract_name_from_binding binding with
        | Some n -> n :: acc
        | None -> acc) in
    match l.rest with
    | None -> names
    | Some rest ->
        (match extract_name_from_binding rest with
        | Some n -> n :: names
        | None -> names)

  and collect_defs_in_statement_list : string list -> statement_list -> string list =
   fun defs l ->
    List.fold_left l ~init:defs ~f:collect_defs_in_statement

  and collect_defs_in_statement : string list -> statement * location -> string list =
   fun defs (s, _) ->
    match s with
    | Block b -> collect_defs_in_statement_list defs b
    | Variable_statement (_, decls) ->
        (* Collect identifiers from variable declarations *)
        List.fold_left decls ~init:defs ~f:(fun defs decl ->
            match decl with
            | DeclIdent (i, _) -> name_of_ident i :: defs
            | DeclPattern _ -> defs)
    | Function_declaration (i, _) ->
        (* Collect function name if named *)
        name_of_ident i :: defs
    | Class_declaration (i, _) ->
        (* Collect class name if named *)
        name_of_ident i :: defs
    | Empty_statement
    | Continue_statement _
    | Break_statement _
    | Return_statement _
    | Debugger_statement
    | Throw_statement _ -> defs
    | Expression_statement _ -> defs
    | If_statement (_, s1, s2) ->
        let defs = collect_defs_in_statement defs s1 in
        (match s2 with
        | None -> defs
        | Some s2 -> collect_defs_in_statement defs s2)
    | Do_while_statement (s, _) -> collect_defs_in_statement defs s
    | While_statement (_, s) -> collect_defs_in_statement defs s
    | For_statement (e1, _, _, s) ->
        let defs =
          match e1 with
          | Left _ -> defs
          | Right (_, decls) ->
              List.fold_left decls ~init:defs ~f:(fun defs decl ->
                  match decl with
                  | DeclIdent (i, _) -> name_of_ident i :: defs
                  | DeclPattern _ -> defs)
        in
        collect_defs_in_statement defs s
    | ForIn_statement (e1, _, s)
    | ForOf_statement (e1, _, s)
    | ForAwaitOf_statement (e1, _, s) ->
        let defs =
          match e1 with
          | Left _ -> defs
          | Right (_, binding) ->
              (match binding with
              | BindingIdent i -> name_of_ident i :: defs
              | BindingPattern _ -> defs)
        in
        collect_defs_in_statement defs s
    | With_statement (_, s) -> collect_defs_in_statement defs s
    | Labelled_statement (_, s) -> collect_defs_in_statement defs s
    | Switch_statement (_, cc, def, cc') ->
        let defs =
          List.fold_left cc ~init:defs ~f:(fun defs (_, sl) ->
              collect_defs_in_statement_list defs sl)
        in
        let defs =
          match def with
          | None -> defs
          | Some def -> collect_defs_in_statement_list defs def
        in
        List.fold_left cc' ~init:defs ~f:(fun defs (_, sl) ->
            collect_defs_in_statement_list defs sl)
    | Try_statement (b, ctch, fin) ->
        let defs = collect_defs_in_statement_list defs b in
        let defs =
          match ctch with
          | None -> defs
          | Some (param, b) ->
              let defs =
                match param with
                | None -> defs
                | Some (binding, _) ->
                    (match binding with
                    | BindingIdent i -> name_of_ident i :: defs
                    | BindingPattern _ -> defs)
              in
              collect_defs_in_statement_list defs b
        in
        (match fin with
        | None -> defs
        | Some b -> collect_defs_in_statement_list defs b)
    | Import _
    | Export _ -> defs

  (* Collect variables used (referenced) in a statement list - returns string list for PHP use clause *)
  and collect_uses_in_statement_list : string list -> statement_list -> string list =
   fun uses l ->
    List.fold_left l ~init:uses ~f:collect_uses_in_statement

  and collect_uses_in_statement : string list -> statement * location -> string list =
   fun uses (s, _) ->
    match s with
    | Block b -> collect_uses_in_statement_list uses b
    | Variable_statement (_, decls) ->
        List.fold_left decls ~init:uses ~f:collect_uses_in_variable_declaration
    | Function_declaration (_, (_, _, body, _)) ->
        collect_uses_in_statement_list uses body
    | Class_declaration (_, { body; _ }) ->
        List.fold_left body ~init:uses ~f:(fun uses elt ->
            match elt with
            | CEMethod (_, _, _, m) ->
                (match m with
                | MethodGet (_, _, body, _)
                | MethodSet (_, _, body, _)
                | Method (_, _, body, _) ->
                    collect_uses_in_statement_list uses body)
            | CEField (_, _, _, Some (e, _)) ->
                collect_uses_in_expression uses e
            | CEAccessor (_, _, _, Some (e, _)) ->
                collect_uses_in_expression uses e
            | CEStaticBLock body ->
                collect_uses_in_statement_list uses body
            | _ -> uses)
    | Empty_statement
    | Continue_statement _
    | Break_statement _
    | Return_statement (None, _)
    | Debugger_statement -> uses
    | Expression_statement e -> collect_uses_in_expression uses e
    | If_statement (e, s1, s2) ->
        let uses = collect_uses_in_expression uses e in
        let uses = collect_uses_in_statement uses s1 in
        (match s2 with
        | None -> uses
        | Some s2 -> collect_uses_in_statement uses s2)
    | Do_while_statement (s, e) ->
        let uses = collect_uses_in_statement uses s in
        collect_uses_in_expression uses e
    | While_statement (e, s) ->
        let uses = collect_uses_in_expression uses e in
        collect_uses_in_statement uses s
    | For_statement (e1, e2, e3, s) ->
        let uses =
          match e1 with
          | Left None -> uses
          | Left (Some e) -> collect_uses_in_expression uses e
          | Right (_, decls) ->
              List.fold_left decls ~init:uses ~f:collect_uses_in_variable_declaration
        in
        let uses =
          match e2 with
          | None -> uses
          | Some e -> collect_uses_in_expression uses e
        in
        let uses =
          match e3 with
          | None -> uses
          | Some e -> collect_uses_in_expression uses e
        in
        collect_uses_in_statement uses s
    | ForIn_statement (e1, e2, s) ->
        let uses =
          match e1 with
          | Left e -> collect_uses_in_expression uses e
          | Right _ -> uses
        in
        let uses = collect_uses_in_expression uses e2 in
        collect_uses_in_statement uses s
    | ForOf_statement (e1, e2, s)
    | ForAwaitOf_statement (e1, e2, s) ->
        let uses =
          match e1 with
          | Left e -> collect_uses_in_expression uses e
          | Right _ -> uses
        in
        let uses = collect_uses_in_expression uses e2 in
        collect_uses_in_statement uses s
    | Return_statement (Some e, _) -> collect_uses_in_expression uses e
    | With_statement (e, s) ->
        let uses = collect_uses_in_expression uses e in
        collect_uses_in_statement uses s
    | Labelled_statement (_, s) -> collect_uses_in_statement uses s
    | Switch_statement (e, cc, def, cc') ->
        let uses = collect_uses_in_expression uses e in
        let uses =
          List.fold_left cc ~init:uses ~f:(fun uses (e, sl) ->
              let uses = collect_uses_in_expression uses e in
              collect_uses_in_statement_list uses sl)
        in
        let uses =
          match def with
          | None -> uses
          | Some def -> collect_uses_in_statement_list uses def
        in
        List.fold_left cc' ~init:uses ~f:(fun uses (e, sl) ->
            let uses = collect_uses_in_expression uses e in
            collect_uses_in_statement_list uses sl)
    | Throw_statement e -> collect_uses_in_expression uses e
    | Try_statement (b, ctch, fin) ->
        let uses = collect_uses_in_statement_list uses b in
        let uses =
          match ctch with
          | None -> uses
          | Some (_, b) -> collect_uses_in_statement_list uses b
        in
        (match fin with
        | None -> uses
        | Some b -> collect_uses_in_statement_list uses b)
    | Import _
    | Export _ -> uses

  and collect_uses_in_variable_declaration : string list -> variable_declaration -> string list =
   fun uses decl ->
    match decl with
    | DeclIdent (_, None) -> uses
    | DeclIdent (_, Some (e, _)) -> collect_uses_in_expression uses e
    | DeclPattern (_, (e, _)) -> collect_uses_in_expression uses e

  and collect_uses_in_expression : string list -> expression -> string list =
   fun uses e ->
    match e with
    | EVar i -> name_of_ident i :: uses
    | ESeq (e1, e2) ->
        let uses = collect_uses_in_expression uses e1 in
        collect_uses_in_expression uses e2
    | ECond (e1, e2, e3) ->
        let uses = collect_uses_in_expression uses e1 in
        let uses = collect_uses_in_expression uses e2 in
        collect_uses_in_expression uses e3
    | EAssignTarget t -> collect_uses_in_assignment_target uses t
    | EBin (_, e1, e2) ->
        let uses = collect_uses_in_expression uses e1 in
        collect_uses_in_expression uses e2
    | EUn (_, e) -> collect_uses_in_expression uses e
    | ECall (e, _, args, _) ->
        let uses = collect_uses_in_expression uses e in
        List.fold_left args ~init:uses ~f:(fun uses arg ->
            match arg with
            | Arg e | ArgSpread e -> collect_uses_in_expression uses e)
    | ECallTemplate (e, t, _) ->
        let uses = collect_uses_in_expression uses e in
        List.fold_left t ~init:uses ~f:(fun uses part ->
            match part with
            | TStr _ -> uses
            | TExp e -> collect_uses_in_expression uses e)
    | EAccess (e, _, e') ->
        let uses = collect_uses_in_expression uses e in
        collect_uses_in_expression uses e'
    | EDot (e, _, _)
    | EDotPrivate (e, _, _) -> collect_uses_in_expression uses e
    | ENew (e, args, _) ->
        let uses = collect_uses_in_expression uses e in
        (match args with
        | None -> uses
        | Some args ->
            List.fold_left args ~init:uses ~f:(fun uses arg ->
                match arg with
                | Arg e | ArgSpread e -> collect_uses_in_expression uses e))
    | EFun (name, (_, params, body, _)) ->
        let body_uses = collect_uses_in_statement_list [] body in
        let body_defs = collect_defs_in_statement_list [] body in
        let param_names = get_param_names params in
        let func_name = match name with 
          | Some i -> [name_of_ident i]
          | _ -> [] in
        let all_local_defs = List.concat [ body_defs; param_names; func_name ] in
        let free_vars = List.filter body_uses ~f:(fun v -> not (List.mem ~eq:String.equal v all_local_defs)) in
        List.append free_vars uses
    | EClass _ -> uses
    | EArrow ((_, params, body, _), _, _) ->
        let body_uses = 
          let b = match body with | [ (Block l, _) ] -> l | l -> l in
          collect_uses_in_statement_list [] b in
        let body_defs = 
          let b = match body with | [ (Block l, _) ] -> l | l -> l in
          collect_defs_in_statement_list [] b in
        let param_names = get_param_names params in
        let all_local_defs = List.concat [ body_defs; param_names ] in
        let free_vars = List.filter body_uses ~f:(fun v -> not (List.mem ~eq:String.equal v all_local_defs)) in
        List.append free_vars uses    | EStr _
    | ETemplate _ -> uses
    | EArr el ->
        List.fold_left el ~init:uses ~f:(fun uses elt ->
            match elt with
            | ElementHole -> uses
            | Element e | ElementSpread e -> collect_uses_in_expression uses e)
    | EBool _
    | ENum _ -> uses
    | EObj lst ->
        List.fold_left lst ~init:uses ~f:(fun uses prop ->
            match prop with
            | Property (_, e) -> collect_uses_in_expression uses e
            | PropertyMethod (_, Method (_, _, _, _))
            | PropertyMethod (_, MethodGet _)
            | PropertyMethod (_, MethodSet _) ->
                collect_uses_in_expression uses e
            | PropertySpread e -> collect_uses_in_expression uses e
            | CoverInitializedName _ -> uses)
    | ERegexp _ -> uses
    | EYield { expr; _ } ->
        (match expr with
        | None -> uses
        | Some e -> collect_uses_in_expression uses e)
    | EPrivName _
    | CoverCallExpressionAndAsyncArrowHead _
    | CoverParenthesizedExpressionAndArrowParameterList _ -> uses

  and collect_uses_in_assignment_target : string list -> assignment_target -> string list =
   fun uses t ->
    match t with
    | ObjectTarget list ->
        List.fold_left list ~init:uses ~f:(fun uses prop ->
            match prop with
            | TargetPropertyId (_, None) -> uses
            | TargetPropertyId (_, Some (e, _)) -> collect_uses_in_expression uses e
            | TargetProperty (_, e, _) -> collect_uses_in_expression uses e
            | TargetPropertySpread e -> collect_uses_in_expression uses e
            | TargetPropertyMethod _ -> uses)
    | ArrayTarget list ->
        List.fold_left list ~init:uses ~f:(fun uses elt ->
            match elt with
            | TargetElementHole -> uses
            | TargetElementId (_, None) -> uses
            | TargetElementId (_, Some (e, _))
            | TargetElement e
            | TargetElementSpread e -> collect_uses_in_expression uses e)

  and argument f a =
    PP.start_group f 0;
    (match a with
    | Arg e -> expression AssignementExpression f e
    | ArgSpread e ->
        PP.string f "...";
        expression AssignementExpression f e);
    PP.end_group f

  and arguments f l = comma_list f ~force_last_comma:(fun _ -> false) argument l

  and variable_declaration f ?(in_ = true) x =
    match x with
    | DeclIdent (i, None) -> ident f ~kind:`Binding i
    | DeclIdent (i, Some (e, loc)) ->
        PP.start_group f 1;
        PP.start_group f 0;
        ident f ~kind:`Binding i;
        PP.space f;
        PP.string f "=";
        PP.end_group f;
        PP.start_group f 1;
        PP.space f;
        output_debug_info f loc;
        let p = (not in_) && contains ~in_:true Expression e in
        if p
        then (
          PP.start_group f 1;
          PP.string f "(");
        expression AssignementExpression f e;
        if p
        then (
          PP.string f ")";
          PP.end_group f);
        PP.end_group f;
        PP.end_group f
    | DeclPattern (p, (e, loc)) ->
        PP.start_group f 1;
        PP.start_group f 0;
        pattern f p;
        PP.space f;
        PP.string f "=";
        PP.end_group f;
        output_debug_info f loc;
        PP.start_group f 1;
        PP.space f;
        let p = (not in_) && contains ~in_:true Expression e in
        if p
        then (
          PP.start_group f 1;
          PP.string f "(");
        expression AssignementExpression f e;
        if p
        then (
          PP.string f ")";
          PP.end_group f);
        PP.end_group f;
        PP.end_group f

  and binding_property f x =
    match x with
    | Prop_binding (pn, e) ->
        property_name f pn;
        PP.string f ":";
        PP.space f;
        binding_element f e
    | Prop_ident (Prop_and_ident i, None) -> ident f ~kind:`Binding i
    | Prop_ident (Prop_and_ident i, Some (e, loc)) ->
        ident f ~kind:`Binding i;
        PP.space f;
        PP.string f "=";
        PP.space f;
        output_debug_info f loc;
        expression AssignementExpression f e

  and binding_element f (b, (e : initialiser option)) =
    match e with
    | None -> binding f b
    | Some (e, loc) ->
        binding f b;
        PP.space f;
        PP.string f "=";
        PP.space f;
        output_debug_info f loc;
        expression AssignementExpression f e

  and binding f x =
    match x with
    | BindingIdent id -> ident f ~kind:`Binding id
    | BindingPattern p -> pattern f p

  and binding_array_elt f x =
    match x with
    | None -> ()
    | Some e -> binding_element f e

  and pattern f p =
    match p with
    | ObjectBinding { list; rest } ->
        PP.start_group f 1;
        PP.string f "{";
        comma_list_rest
          f
          ~force_last_comma:(fun _ -> false)
          binding_property
          list
          (ident ~kind:`Binding)
          rest;
        PP.string f "}";
        PP.end_group f
    | ArrayBinding { list; rest } ->
        PP.start_group f 1;
        PP.string f "[";
        comma_list_rest
          f
          ~force_last_comma:(function
            | None -> true
            | Some _ -> false)
          binding_array_elt
          list
          binding
          rest;
        PP.string f "]";
        PP.end_group f

  and variable_declaration_list_aux f ?in_ l =
    match l with
    | [] -> assert false
    | [ d ] -> variable_declaration f ?in_ d
    | d :: r ->
        variable_declaration f ?in_ d;
        if Config.Flag.php_output () then PP.string f ";" else PP.string f ",";
        PP.space f;
        variable_declaration_list_aux f ?in_ r

  and variable_declaration_kind f kind =
    match kind with
    | Var ->
        if not (Config.Flag.php_output ())
        then (
          PP.string f "var";
          PP.space f)
    | Let ->
        if not (Config.Flag.php_output ())
        then (
          PP.string f "let";
          PP.space f)
    | Const ->
        if not (Config.Flag.php_output ())
        then (
          PP.string f "const";
          PP.space f)
    | Using ->
        PP.string f "using";
        PP.non_breaking_space f
    | AwaitUsing ->
        PP.string f "await using";
        PP.non_breaking_space f

  and variable_declaration_list ?in_ kind close f = function
    | [] -> ()
    | [ x ] ->
        PP.start_group f 1;
        variable_declaration_kind f kind;
        variable_declaration f ?in_ x;
        if close then PP.string f ";";
        PP.end_group f
    | l ->
        PP.start_group f 1;
        variable_declaration_kind f kind;
        variable_declaration_list_aux f ?in_ l;
        if close then PP.string f ";";
        PP.end_group f

  and parenthesized_expression
      ?(last_semi = fun () -> ())
      ?(obj = false)
      ?(funct = false)
      ?(class_ = false)
      ?(let_identifier = false)
      ?(async_identifier = false)
      ?(force = false)
      l
      f
      e =
    if force || starts_with ~obj ~funct ~class_ ~let_identifier ~async_identifier l e
    then (
      PP.start_group f 1;
      PP.string f "(";
      expression l f e;
      PP.string f ")";
      last_semi ();
      PP.end_group f)
    else (
      PP.start_group f 0;
      expression l f e;
      last_semi ();
      PP.end_group f)

  and for_binding f k v =
    variable_declaration_kind f k;
    binding f v

  and statement1 ?last f s =
    match s with
    | Block _, _ -> statement ?last f s
    | _ ->
        PP.space ~indent:1 f;
        PP.start_group f 0;
        statement ?last f s;
        PP.end_group f

  and statement ?(last = false) f (s, loc) =
    let can_omit_semi = PP.compact f && last in
    let last_semi ?(ret = false) () =
      if can_omit_semi
      then ()
      else if ret && source_map_enabled && PP.compact f
      then
        (* In Chrome, the debugger will stop right after a return
           statement. We want a whitespace between this statement and
           the next one to avoid confusing this location and the
           location of the next statement. When pretty-printing, this
           is already the case. In compact mode, we add a newline. *)
        PP.string f ";\n"
      else PP.string f ";"
    in
    if stop_on_statement s then output_debug_info f loc;
    match s with
    | Block b -> block f b
    | Variable_statement (k, l) ->
        if Config.Flag.php_output () && List.length l > 1
        then (
          PP.start_group f 1;
          PP.string f "{";
          PP.break f;
          variable_declaration_list k true f l;
          PP.end_group f;
          PP.break f;
          PP.string f "}")
        else variable_declaration_list k (not can_omit_semi) f l
    | Function_declaration (i, decl) -> function_declaration' f (Some i) decl
    | Class_declaration (i, cl_decl) -> class_declaration f (Some i) cl_decl
    | Empty_statement -> PP.string f ";"
    | Debugger_statement ->
        PP.string f "debugger";
        last_semi ()
    | Expression_statement e ->
        (* Parentheses are required when the expression
           starts syntactically with "{", "function", "async function"
           or "let [" *)
        parenthesized_expression
          ~last_semi
          ~obj:true
          ~funct:true
          ~class_:true
          ~let_identifier:true
          Expression
          f
          e
    | If_statement (e, s1, (Some _ as s2)) when ends_with_if_without_else s1 ->
        (* Dangling else issue... *)
        statement ~last f (If_statement (e, (Block [ s1 ], N), s2), N)
    | If_statement (e, s1, s2) ->
        let rec ite kw e s1 s2 =
          (let last_in_s1 =
             match s2 with
             | None -> Some last
             | Some _ -> None
           in
           PP.start_group f 0;
           PP.start_group f 1;
           PP.string f kw;
           PP.break f;
           PP.start_group f 1;
           PP.string f "(";
           expression Expression f e;
           PP.string f ")";
           PP.end_group f;
           PP.end_group f;
           statement1 ?last:last_in_s1 f s1);
          match s2 with
          | None -> PP.end_group f
          | Some (If_statement (e, s1, s2), _) when not (ends_with_if_without_else s1) ->
              PP.space f;
              ite "else if" e s1 s2;
              PP.end_group f
          | Some s2 ->
              PP.space f;
              PP.string f "else";
              statement1 ~last f s2;
              PP.end_group f
        in
        ite "if" e s1 s2
    | While_statement (e, s) ->
        PP.start_group f 0;
        PP.start_group f 0;
        PP.string f "while";
        PP.break f;
        PP.start_group f 1;
        PP.string f "(";
        expression Expression f e;
        PP.string f ")";
        PP.end_group f;
        PP.end_group f;
        statement1 ~last f s;
        PP.end_group f
    | Do_while_statement (s, e) ->
        PP.start_group f 0;
        PP.string f "do";
        statement1 f s;
        PP.break f;
        PP.string f "while";
        PP.break1 f;
        PP.start_group f 1;
        PP.string f "(";
        expression Expression f e;
        PP.string f ")";
        last_semi ();
        PP.end_group f;
        PP.end_group f
    | For_statement (e1, e2, e3, s) ->
        PP.start_group f 0;
        PP.start_group f 0;
        PP.string f "for";
        PP.break f;
        PP.start_group f 1;
        PP.string f "(";
        (match e1 with
        | Left None -> ()
        | Left (Some e) ->
            (* Should not starts with "let [" and should not contain "in" *)
            let force = contains ~in_:true Expression e in
            parenthesized_expression ~force ~let_identifier:true Expression f e
        | Right (k, l) -> variable_declaration_list k ~in_:false false f l);
        PP.string f ";";
        (match e2 with
        | None -> ()
        | Some e2 ->
            PP.space f;
            expression Expression f e2);
        PP.string f ";";
        (match e3 with
        | None -> ()
        | Some e3 ->
            PP.space f;
            expression Expression f e3);
        PP.string f ")";
        PP.end_group f;
        PP.end_group f;
        statement1 ~last f s;
        PP.end_group f
    | ForIn_statement (e1, e2, s) ->
        PP.start_group f 0;
        PP.start_group f 0;
        PP.string f "for";
        PP.break f;
        PP.start_group f 1;
        PP.string f "(";
        (match e1 with
        | Left e ->
            (* Should not starts with "let [" *)
            parenthesized_expression ~let_identifier:true LeftHandSideExpression f e
        | Right (k, v) -> for_binding f k v);
        PP.space f;
        PP.string f "in";
        PP.break f;
        PP.space f;
        expression Expression f e2;
        PP.string f ")";
        PP.end_group f;
        PP.end_group f;
        statement1 ~last f s;
        PP.end_group f
    | ForOf_statement (e1, e2, s) ->
        PP.start_group f 0;
        PP.start_group f 0;
        PP.string f "for";
        PP.break f;
        PP.start_group f 1;
        PP.string f "(";
        (match e1 with
        | Left e ->
            (* Should not starts with "let" or "async of" *)
            parenthesized_expression
              ~let_identifier:true
              ~async_identifier:true
              LeftHandSideExpression
              f
              e
        | Right (k, v) -> for_binding f k v);
        PP.space f;
        PP.string f "of";
        PP.break f;
        PP.space f;
        expression AssignementExpression f e2;
        PP.string f ")";
        PP.end_group f;
        PP.end_group f;
        statement1 ~last f s;
        PP.end_group f
    | ForAwaitOf_statement (e1, e2, s) ->
        PP.start_group f 0;
        PP.start_group f 0;
        PP.string f "for await";
        PP.break f;
        PP.start_group f 1;
        PP.string f "(";
        (match e1 with
        | Left e ->
            (* Should not starts with "let" *)
            parenthesized_expression ~let_identifier:true LeftHandSideExpression f e
        | Right (k, v) -> for_binding f k v);
        PP.space f;
        PP.string f "of";
        PP.break f;
        PP.space f;
        expression AssignementExpression f e2;
        PP.string f ")";
        PP.end_group f;
        PP.end_group f;
        statement1 ~last f s;
        PP.end_group f
    | Continue_statement None ->
        PP.string f "continue";
        last_semi ()
    | Continue_statement (Some s) ->
        let (Utf8 l) = name_of_label s in
        if Config.Flag.php_output () then (PP.string f "goto "; identName f (get_php_label l "cont"))
        else (PP.string f "continue "; identName f l);
        last_semi ()
    | Break_statement None ->
        PP.string f "break";
        last_semi ()
    | Break_statement (Some s) ->
        let (Utf8 l) = name_of_label s in
        if Config.Flag.php_output ()
        then (
          PP.string f "goto ";
          identName f (get_php_label l "break");
          last_semi ())
        else (
          PP.string f "break ";
          identName f l;
          last_semi ())
    | Return_statement (e, loc) -> (
        match e with
        | None ->
            PP.string f "return";
            output_debug_info f loc;
            last_semi ~ret:true ()
        | Some (EFun (i, ({ async = false; generator = false }, l, b, pc))) when not (Config.Flag.php_output ()) ->
            PP.start_group f 1;
            PP.start_group f 0;
            PP.start_group f 0;
            PP.string f "return function";
            opt_identifier f ~kind:`Binding i;
            PP.end_group f;
            PP.break f;
            PP.start_group f 1;
            PP.string f "(";
            formal_parameter_list f l;
            PP.string f ")";
            PP.end_group f;
            PP.end_group f;
            PP.string f "{";
            PP.break f;
            function_body f b;
            output_debug_info f pc;
            PP.string f "}";
            output_debug_info f loc;
            last_semi ~ret:true ();
            PP.end_group f
        | Some e ->
            PP.start_group f 7;
            PP.string f "return";
            PP.non_breaking_space f;
            PP.start_group f 0;
            expression Expression f e;
            output_debug_info f loc;
            last_semi ~ret:true ();
            PP.end_group f;
            PP.end_group f
            (* There MUST be a space between the return and its
               argument. A line return will not work *)
        )
    | Labelled_statement (i, s) ->
        let (Utf8 l) = name_of_label i in
        if Config.Flag.php_output ()
        then (
          (* PHP doesn't support labeled blocks, only labeled loops *)
          let old_map = !php_label_map in
          let cont_l = unique_php_label ("cont_" ^ l) in
          let break_l = unique_php_label ("break_" ^ l) in
          php_label_map := ((l, "cont"), cont_l) :: ((l, "break"), break_l) :: !php_label_map;
          match s with
          | (While_statement (_, _)
            | For_statement (_, _, _, _)
            | ForIn_statement (_, _, _)
            | ForOf_statement (_, _, _)
            | ForAwaitOf_statement (_, _, _)
            | Do_while_statement (_, _)), _ ->
              (* For loops, keep the label at start for continue/goto *)
              identName f cont_l;
              PP.string f ":";
              PP.space f;
              statement ~last f s;
              PP.break f;
              identName f break_l;
              PP.string f ": ;";
              php_label_map := old_map
          | _ ->
              (* For other statements (if, blocks, etc.) *)
              identName f cont_l;
              PP.string f ":";
              PP.space f;
              statement ~last f s;
              PP.break f;
              identName f break_l;
              PP.string f ": ;";
              php_label_map := old_map)
        else (
          identName f l;
          PP.string f ":";
          PP.space f;
          statement ~last f s)
    | Switch_statement (e, cc, def, cc') ->
        PP.start_group f 1;
        PP.start_group f 0;
        PP.string f "switch";
        PP.break f;
        PP.start_group f 1;
        PP.string f "(";
        expression Expression f e;
        PP.string f ")";
        PP.end_group f;
        PP.end_group f;
        PP.start_group f 1;
        PP.string f "{";
        PP.break f;
        let output_one last (e, sl) =
          PP.start_group f 1;
          PP.string f "case";
          PP.space f;
          expression Expression f e;
          PP.string f ":";
          PP.end_group f;
          PP.start_group f 1;
          (match sl with
          | _ :: _ -> PP.space f
          | [] -> PP.break f);
          PP.start_group f 0;
          statement_list ~skip_last_semi:last f sl;
          PP.end_group f;
          PP.end_group f
        in
        let rec loop last = function
          | [] -> ()
          | [ x ] ->
              output_one last x;
              if not last then PP.break f
          | x :: xs ->
              output_one false x;
              PP.break f;
              loop last xs
        in
        loop (Option.is_none def && List.is_empty cc') cc;
        (match def with
        | None -> ()
        | Some def ->
            PP.start_group f 1;
            PP.string f "default:";
            PP.space f;
            PP.start_group f 0;
            let last = List.is_empty cc' in
            statement_list ~skip_last_semi:last f def;
            PP.end_group f;
            PP.end_group f;
            if not last then PP.break f);
        loop true cc';
        PP.end_group f;
        PP.end_group f;
        PP.break f;
        PP.string f "}"
    | Throw_statement e ->
        PP.start_group f 6;
        PP.string f "throw";
        PP.non_breaking_space f;
        PP.start_group f 0;
        (if Config.Flag.php_output ()
         then (
           (* PHP only allows Throwable objects to be thrown *)
           PP.string f "new CamlException(";
           expression Expression f e;
           PP.string f ")")
         else expression Expression f e);
        last_semi ();
        PP.end_group f;
        PP.end_group f
    (* There must be a space between the return and its
       argument. A line return would not work *)
    | Try_statement (b, ctch, fin) ->
        PP.start_group f 0;
        PP.string f "try";
        block f b;
        (match ctch with
        | None -> ()
        | Some (i, b) ->
            PP.break f;
            (match i with
            | None -> PP.string f "catch"
            | Some i ->
                PP.string f "catch(";
                if Config.Flag.php_output () then PP.string f "Throwable ";
                formal_parameter f i;
                PP.string f ")");
            block f b);
        (match fin with
        | None -> ()
        | Some b ->
            PP.break f;
            PP.string f "finally";
            block f b);
        PP.end_group f
    | With_statement (e, s) ->
        PP.start_group f 0;
        PP.string f "with(";
        expression Expression f e;
        PP.string f ")";
        PP.break f;
        statement f s;
        PP.end_group f
    | Import ({ kind; from; withClause }, _loc) ->
        PP.start_group f 0;
        PP.string f "import";
        (match kind with
        | SideEffect -> ()
        | Default i ->
            PP.space f;
            ident f ~kind:`Binding i
        | DeferNamespace i ->
            PP.space f;
            PP.string f "defer";
            PP.space f;
            PP.string f "* as ";
            ident f ~kind:`Binding i
        | Namespace (def, i) ->
            Option.iter def ~f:(fun def ->
                PP.space f;
                ident f ~kind:`Binding def;
                PP.string f ",");
            PP.space f;
            PP.string f "* as ";
            ident f ~kind:`Binding i
        | Named (def, l) ->
            Option.iter def ~f:(fun def ->
                PP.space f;
                ident f ~kind:`Binding def;
                PP.string f ",");
            PP.space f;
            PP.string f "{";
            PP.space f;
            comma_list
              f
              ~force_last_comma:(fun _ -> false)
              (fun f (s, i) ->
                if
                  match i with
                  | S { name; _ } when Stdlib.Utf8_string.equal name s -> true
                  | _ -> false
                then ident f ~kind:`Binding i
                else (
                  pp_ident_or_string_lit f s;
                  PP.string f " as ";
                  ident f ~kind:`Binding i))
              l;
            PP.space f;
            PP.string f "}");
        (match kind with
        | SideEffect -> ()
        | _ ->
            PP.space f;
            PP.string f "from");
        PP.space f;
        pp_string_lit f from;
        (match withClause with
        | None -> ()
        | Some l ->
            PP.space f;
            PP.string f "with";
            PP.space f;
            PP.string f "{";
            PP.space f;
            comma_list
              ~force_last_comma:(fun _ -> false)
              f
              (fun f (i, s) ->
                pp_ident_or_string_lit f i;
                PP.string f " : ";
                pp_string_lit f s)
              l;
            PP.space f;
            PP.string f "}");
        PP.string f ";";
        PP.end_group f
    | Export (e, _loc) ->
        PP.start_group f 0;
        (* Print decorators before 'export' for decorated class exports *)
        (match e with
        | ExportClass (_, decl) | ExportDefaultClass (_, decl) ->
            decorator_list f decl.decorators
        | ExportVar _
        | ExportFun _
        | ExportNames _
        | ExportDefaultFun _
        | ExportDefaultExpression _
        | ExportFrom _
        | CoverExportFrom _ -> ());
        PP.string f "export";
        (match e with
        | ExportNames l ->
            PP.space f;
            PP.string f "{";
            PP.space f;
            comma_list
              ~force_last_comma:(fun _ -> false)
              f
              (fun f (i, s) ->
                if
                  match i with
                  | S { name; _ } when Stdlib.Utf8_string.equal name s -> true
                  | _ -> false
                then ident f ~kind:`Reference i
                else (
                  ident f ~kind:`Reference i;
                  PP.string f " as ";
                  pp_ident_or_string_lit f s))
              l;
            PP.space f;
            PP.string f "};"
        | ExportFrom { kind; from; withClause } ->
            PP.space f;
            (match kind with
            | Export_all None -> PP.string f "*"
            | Export_all (Some s) ->
                PP.string f "* as ";
                pp_ident_or_string_lit f s
            | Export_names l ->
                PP.string f "{";
                PP.space f;
                comma_list
                  ~force_last_comma:(fun _ -> false)
                  f
                  (fun f (a, b) ->
                    if Stdlib.Utf8_string.equal a b
                    then pp_ident_or_string_lit f a
                    else (
                      pp_ident_or_string_lit f a;
                      PP.string f " as ";
                      pp_ident_or_string_lit f b))
                  l;
                PP.space f;
                PP.string f "}");
            PP.space f;
            PP.string f "from";
            PP.space f;
            pp_string_lit f from;
            (match withClause with
            | None -> ()
            | Some l ->
                PP.space f;
                PP.string f "with";
                PP.space f;
                PP.string f "{";
                PP.space f;
                comma_list
                  ~force_last_comma:(fun _ -> false)
                  f
                  (fun f (i, s) ->
                    pp_ident_or_string_lit f i;
                    PP.string f " : ";
                    pp_string_lit f s)
                  l;
                PP.space f;
                PP.string f "}");

            PP.string f ";"
        | ExportDefaultExpression e ->
            PP.space f;
            PP.string f "default";
            PP.space f;
            parenthesized_expression
              ~last_semi
              ~obj:true
              ~funct:true
              ~class_:true
              ~let_identifier:true
              Expression
              f
              e
        | ExportDefaultFun (i, decl) ->
            PP.space f;
            PP.string f "default";
            PP.space f;
            function_declaration' f i decl
        | ExportDefaultClass (id, decl) ->
            PP.space f;
            PP.string f "default";
            PP.space f;
            (* Decorators already printed before 'export' *)
            class_declaration f id { decl with decorators = [] }
        | ExportFun (id, decl) ->
            PP.space f;
            function_declaration' f (Some id) decl
        | ExportClass (id, decl) ->
            PP.space f;
            (* Decorators already printed before 'export' *)
            class_declaration f (Some id) { decl with decorators = [] }
        | ExportVar (k, l) ->
            PP.space f;
            variable_declaration_list k (not can_omit_semi) f l
        | CoverExportFrom e -> early_error e);
        PP.end_group f

  and statement_list f ?skip_last_semi b =
    match b with
    | [] -> ()
    | [ s ] -> statement f ?last:skip_last_semi s
    | s :: r ->
        statement f s;
        PP.space f;
        statement_list f ?skip_last_semi r

  and block f b =
    PP.start_group f 0;
    PP.start_group f 1;
    PP.string f "{";
    PP.break f;
    statement_list ~skip_last_semi:true f b;
    PP.end_group f;
    PP.break f;
    PP.string f "}";
    PP.end_group f

  and function_declaration : type a.
      'pp -> string -> ('pp -> a -> unit) -> a option -> _ -> _ -> _ -> unit =
   fun f prefix (pp_name : _ -> a -> unit) (name : a option) l body loc ->
    let is_php = Config.Flag.php_output () in
    let old_php_label_map = !php_label_map in
    if is_php then php_label_map := [];
    let has_name = Option.is_some name in

    (* Calculate free variables for PHP use clause *)
    let free_vars : string list =
      if is_php
      then (
        (* Collect uses from function body *)
        let body_uses = collect_uses_in_statement_list [] body in
        (* Collect defs from function body *)
        let body_defs = collect_defs_in_statement_list [] body in
        (* Get parameter names *)
        let param_names = get_param_names l in
        (* Free vars = Uses \ (Defs U Params U Globals) *)
        let all_defs = List.concat [ body_defs; param_names ] in
        let free_vars = List.filter body_uses ~f:(fun v ->
            not (List.mem ~eq:String.equal v all_defs)
            && not (String.equal v "globalThis")
            && not (String.equal v "undefined")
            && not (String.equal v "NaN")
            && not (String.equal v "Infinity")
            && not (StringLabels.starts_with ~prefix:"caml_" v)) in
        free_vars
      )
      else []
    in

    PP.start_group f 0;
    PP.start_group f 0;
    PP.start_group f 0;

    if is_php && has_name
    then (
      (* PHP: $name = function instead of function name *)
      PP.string f "$";
      Option.iter name ~f:(fun n -> pp_name f n);
      PP.string f " = ";
      PP.string f prefix)
    else (
      PP.string f prefix;
      match name with
      | None -> ()
      | Some name ->
          if not (String.is_empty prefix) then PP.space f;
          pp_name f name);
    PP.end_group f;
    PP.break f;
    PP.start_group f 1;
    PP.string f "(";
    formal_parameter_list f l;
    PP.string f ")";

    (* Add use clause for PHP (both named and anonymous functions) *)
    if is_php && not (List.is_empty free_vars)
    then (
      PP.string f " use (";
      let vars = List.sort_uniq ~cmp:String.compare free_vars in
      let rec output_vars = function
        | [] -> ()
        | [ v ] ->
            PP.string f "&$";
            PP.string f (sanitize_php v)
        | v :: rest ->
            PP.string f "&$";
            PP.string f (sanitize_php v);
            PP.string f ", ";
            output_vars rest
      in
      output_vars vars;
      PP.string f ")");
    
    PP.end_group f;
    PP.start_group f 1;
    PP.string f "{";
    PP.break f;
    if Config.Flag.php_output () then (
      let body_defs = collect_defs_in_statement_list [] body in
      let param_names = get_param_names l in
      let locals_to_init = List.filter (List.sort_uniq ~cmp:String.compare body_defs) ~f:(fun v ->
          not (List.mem ~eq:String.equal v param_names)
          && not (List.mem ~eq:String.equal v free_vars)
          && not (StringLabels.starts_with ~prefix:"caml_" v)) in
      List.iter locals_to_init ~f:(fun v -> PP.string f ("$" ^ sanitize_php v ^ " = null;"); PP.break f));
    function_body f body;
    PP.end_group f;
    PP.break f;
    output_debug_info f loc;
    PP.string f "}";
    
    (* Add semicolon after function assignment in PHP mode *)
    if is_php && has_name then PP.string f ";";
    
    if is_php then php_label_map := old_php_label_map;
    PP.end_group f

  and function_declaration' f (name : _ option) (k, l, b, loc') =
    let prefix =
      match k with
      | { async = false; generator = false } -> "function"
      | { async = true; generator = false } -> "async function"
      | { async = true; generator = true } -> "async function*"
      | { async = false; generator = true } -> "function*"
    in
    function_declaration f prefix (ident ~prefix:false ~kind:`Binding) name l b loc'

  and decorator f e =
    PP.start_group f 2;
    PP.string f "@";
    expression LeftHandSideExpression f e;
    PP.end_group f

  and decorator_list f decorators =
    match decorators with
    | [] -> ()
    | _ ->
        PP.start_group f 0;
        List.iter decorators ~f:(fun d ->
            decorator f d;
            PP.space f);
        PP.end_group f

  and class_declaration f i x =
    PP.start_group f 0;
    decorator_list f x.decorators;
    PP.start_group f 0;
    PP.start_group f 0;
    PP.string f "class";
    (match i with
    | None -> ()
    | Some i ->
        PP.space f;
        ident ~prefix:false f ~kind:`Binding i);
    PP.end_group f;
    Option.iter x.extends ~f:(fun e ->
        PP.space f;
        PP.string f "extends";
        PP.space f;
        expression LeftHandSideExpression f e;
        PP.space f);
    PP.end_group f;
    PP.start_group f 2;
    PP.string f "{";
    PP.break f;
    List.iter_last x.body ~f:(fun last x ->
        (match x with
        | CEMethod (decorators, static, n, m) ->
            PP.start_group f 0;
            decorator_list f decorators;
            if static
            then (
              PP.string f "static";
              PP.space f);
            method_ f class_element_name n m;
            PP.end_group f
        | CEField (decorators, static, n, i) ->
            PP.start_group f 0;
            decorator_list f decorators;
            if static
            then (
              PP.string f "static";
              PP.space f);
            class_element_name f n;
            (match i with
            | None -> ()
            | Some (e, loc) ->
                PP.space f;
                PP.string f "=";
                PP.space f;
                output_debug_info f loc;
                expression AssignementExpression f e);
            PP.string f ";";
            PP.end_group f
        | CEAccessor (decorators, static, n, i) ->
            PP.start_group f 0;
            decorator_list f decorators;
            if static
            then (
              PP.string f "static";
              PP.space f);
            PP.string f "accessor";
            PP.space f;
            class_element_name f n;
            (match i with
            | None -> ()
            | Some (e, loc) ->
                PP.space f;
                PP.string f "=";
                PP.space f;
                output_debug_info f loc;
                expression AssignementExpression f e);
            PP.string f ";";
            PP.end_group f
        | CEStaticBLock l ->
            PP.start_group f 0;
            PP.string f "static";
            PP.space f;
            block f l;
            PP.end_group f);
        if not last then PP.break f);
    PP.end_group f;
    PP.break f;
    PP.string f "}";
    PP.end_group f

  and class_element_name f x =
    match x with
    | PropName n -> property_name f n
    | PrivName (Utf8 i) ->
        PP.string f "#";
        PP.string f i

  and program f s = statement_list f s
end

let part_of_ident =
  let a =
    Array.init 256 ~f:(fun i ->
        match Char.chr i with
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '$' -> true
        | _ -> false)
  in
  fun c -> Array.unsafe_get a (Char.code c)

let need_space a b =
  (* do not concat 2 different identifier *)
  (part_of_ident a && part_of_ident b)
  (* do not generate end_of_line_comment.
     handle the case of "num / /* comment */ b " *)
  ||
  match a, b with
  | '/', '/'
  (* https://github.com/ocsigen/js_of_ocaml/issues/507 *)
  | '-', '-'
  | '+', '+' -> true
  (* never emit html comments <!-- or --> *)
  | '-', '>' -> true
  | '<', '!' -> true
  | _, _ -> false

let hashtbl_to_list htb =
  String.Hashtbl.fold (fun k v l -> (k, v) :: l) htb []
  |> List.sort ~cmp:(fun (_, a) (_, b) -> compare a b)
  |> List.map ~f:fst

let blackbox_filename = "/builtin/blackbox.ml"

let program ?(accept_unnamed_var = false) ?(source_map = false) f p =
  let temp_mappings = ref [] in
  let files = String.Hashtbl.create 17 in
  let names = String.Hashtbl.create 17 in
  let push_mapping, get_file_index, get_name_index =
    ( (fun pos m -> temp_mappings := (pos, m) :: !temp_mappings)
    , (fun file ->
        try String.Hashtbl.find files file
        with Not_found ->
          let pos = String.Hashtbl.length files in
          String.Hashtbl.add files file pos;
          pos)
    , fun name ->
        try String.Hashtbl.find names name
        with Not_found ->
          let pos = String.Hashtbl.length names in
          String.Hashtbl.add names name pos;
          pos )
  in
  let hidden_location =
    Source_map.Gen_Ori
      { gen_line = -1
      ; gen_col = -1
      ; ori_source = get_file_index blackbox_filename
      ; ori_line = 1
      ; ori_col = 0
      }
  in
  let module O = Make (struct
    let push_mapping = push_mapping

    let get_name_index = get_name_index

    let get_file_index = get_file_index

    let hidden_location = hidden_location

    let source_map_enabled = source_map

    let accept_unnamed_var = accept_unnamed_var
  end) in
  PP.set_needed_space_function f need_space;
  (match Config.effects () with
  | `Cps | `Double_translation -> PP.set_adjust_indentation_function f (fun n -> n mod 40)
  | `Disabled | `Jspi | (exception Failure _) -> ());
  PP.start_group f 0;
  O.program f p;
  PP.end_group f;
  PP.newline f;
  let sm =
    match source_map with
    | false -> { Source_map.sources = []; names = []; mappings = [] }
    | true ->
        let sources = hashtbl_to_list files in
        let names = hashtbl_to_list names in
        let relocate pos m =
          let gen_line = pos.PP.p_line + 1 in
          let gen_col = pos.PP.p_col in
          match m with
          | Source_map.Gen { gen_col = _; gen_line = _ } ->
              Source_map.Gen { gen_col; gen_line }
          | Source_map.Gen_Ori m -> Source_map.Gen_Ori { m with gen_line; gen_col }
          | Source_map.Gen_Ori_Name m ->
              Source_map.Gen_Ori_Name { m with gen_line; gen_col }
        in
        let rec build_mappings pos mapping prev_mappings =
          match mapping with
          | [] -> prev_mappings
          | (pos', m) :: rem ->
              (* Firefox assumes that a mapping stops at the end of a
                 line, which is inconvenient. When this happens, we
                 repeat the mapping on the next line. *)
              if
                pos'.PP.p_line = pos.PP.p_line
                || (pos'.p_line = pos.p_line - 1 && pos.p_col = 0)
              then build_mappings pos' rem (relocate pos' m :: prev_mappings)
              else if pos.p_col > 0
              then
                let pos = { pos with p_col = 0 } in
                build_mappings pos mapping (relocate pos m :: prev_mappings)
              else
                let pos = { pos with p_line = pos.p_line - 1 } in
                build_mappings pos mapping (relocate pos m :: prev_mappings)
        in
        let mappings =
          match !temp_mappings with
          | [] -> []
          | (pos, m) :: rem -> build_mappings pos rem [ relocate pos m ]
        in
        { Source_map.sources; names; mappings }
  in
  PP.check f;
  (if stats ()
   then
     let size i = Printf.sprintf "%.2fKo" (float_of_int i /. 1024.) in
     let _percent n d =
       Printf.sprintf "%.1f%%" (float_of_int n *. 100. /. float_of_int d)
     in
     let total_s = PP.total f in
     Format.eprintf "total size : %s@." (size total_s));
  sm

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
open! Stdlib

module Addr : sig
  type t = int

  val to_string : t -> string

  val zero : t

  val succ : t -> t

  val pred : t -> t

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t
end

module DebugAddr : sig
  type t = private int

  val of_addr : Addr.t -> t

  val to_addr : t -> Addr.t

  val no : t
end

module Var : sig
  type t

  val print : Format.formatter -> t -> unit

  val equal : t -> t -> bool

  val idx : t -> int

  val of_idx : int -> t

  val to_string : ?origin:t -> t -> string

  val fresh : unit -> t

  val fresh_n : string -> t

  val fork : t -> t

  val count : unit -> int

  val compare : t -> t -> int

  val loc : t -> Parse_info.t -> unit

  val get_loc : t -> Parse_info.t option

  val get_name : t -> string option

  val name : t -> string -> unit

  val propagate_name : t -> t -> unit

  val reset : unit -> unit

  val set_pretty : bool -> unit

  val set_stable : bool -> unit

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t

  module Tbl : sig
    type key = t

    type 'a t

    type size = unit

    val get : 'a t -> key -> 'a

    val set : 'a t -> key -> 'a -> unit

    val make : size -> 'a -> 'a t
  end

  module ISet : sig
    type elt = t

    type t

    val empty : unit -> t

    val iter : (elt -> unit) -> t -> unit

    val mem : t -> elt -> bool

    val add : t -> elt -> unit

    val remove : t -> elt -> unit

    val copy : t -> t
  end
end

type cont = Addr.t * Var.t list

type prim =
  | Vectlength
  | Array_get
  | Extern of string
  | Not
  | IsInt
  | Eq
  | Neq
  | Lt
  | Le
  | Ult

type array_or_not =
  | Array
  | NotArray
  | Unknown

type constant =
  | String of string
  | NativeString of string
  | Float of float
  | Float_array of float array
  | Int64 of int64
  | Tuple of int * constant array * array_or_not
  | Int of int32

val constant_equal : constant -> constant -> bool option

type prim_arg =
  | Pv of Var.t
  | Pc of constant

type expr =
  | Apply of Var.t * Var.t list * bool
  (* if true, then # of arguments = # of parameters *)
  | Block of int * Var.t array * array_or_not
  | Field of Var.t * int
  | Closure of Var.t list * cont
  | Constant of constant
  | Prim of prim * prim_arg list

type instr =
  | Let of Var.t * expr
  | Assign of Var.t * Var.t
  | Set_field of Var.t * int * Var.t
  | Offset_ref of Var.t * int
  | Array_set of Var.t * Var.t * Var.t

type last =
  | Return of Var.t
  | Raise of Var.t * [ `Normal | `Notrace | `Reraise ]
  | Stop
  | Branch of cont
  | Cond of Var.t * cont * cont
  | Switch of Var.t * cont array * cont array
  | Pushtrap of cont * Var.t * cont * Addr.Set.t
  | Poptrap of cont

type block =
  { params : Var.t list
  ; body : instr list
  ; branch : last
  }

type program =
  { start : Addr.t
  ; blocks : block Addr.Map.t
  ; free_pc : Addr.t
  }

module Print : sig
  type xinstr =
    | Instr of instr
    | Last of last

  val var_list : Format.formatter -> Var.t list -> unit

  val instr : Format.formatter -> instr -> unit

  val block : (Addr.Map.key -> xinstr -> string) -> int -> block -> unit

  val program : (Addr.Map.key -> xinstr -> string) -> program -> unit

  val last : Format.formatter -> last -> unit

  val cont : Format.formatter -> cont -> unit
end

type 'c fold_blocs = block Addr.Map.t -> Addr.t -> (Addr.t -> 'c -> 'c) -> 'c -> 'c

type fold_blocs_poly = { fold : 'a. 'a fold_blocs } [@@unboxed]

val fold_closures :
  program -> (Var.t option -> Var.t list -> cont -> 'd -> 'd) -> 'd -> 'd

val fold_children : 'c fold_blocs

val traverse :
  fold_blocs_poly -> (Addr.t -> 'c -> 'c) -> Addr.t -> block Addr.Map.t -> 'c -> 'c

val preorder_traverse :
  fold_blocs_poly -> (Addr.t -> 'c -> 'c) -> Addr.t -> block Addr.Map.t -> 'c -> 'c

val prepend : program -> instr list -> program

val empty : program

val is_empty : program -> bool

val eq : program -> program -> bool

val invariant : program -> unit

(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Access paths *)

type t =
    Pident of Ident.t
  | Pdot of t * string * int
  | Papply of t * t

val mk_ident: string -> t
val mk_persistent: string -> t

val same: t -> t -> bool
val isfree: Ident.t -> t -> bool
val binding_time: t -> int

val nopos: int

val name: ?paren:(string -> bool) -> t -> string
    (* [paren] tells whether a path suffix needs parentheses *)
val head: t -> Ident.t

val heads: t -> Ident.t list

val last: t -> string

type typath =
  | Regular of t
  | Ext of t * string
  | LocalExt of Ident.t
  | Cstr of t * string

val constructor_typath: t -> typath
val is_constructor_typath: t -> bool

val ident_name: t -> string option
val ident_name_crash: t -> string
val ident_name_fail: t -> string
val unique_ident_name: t -> string option
val unique_ident_name_crash: t -> string
val is_ident: t -> bool

val unique_name: t -> string



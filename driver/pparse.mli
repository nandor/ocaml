(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*         Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt       *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Driver for the parser, external preprocessors and ast plugin hooks *)

open Format

type error =
  | CannotRun of string
  | WrongMagic of string

exception Error of error

val preprocess : preprocessor:string option -> string -> string
val remove_preprocessed : preprocessor:string option -> string -> unit

type 'a ast_kind =
| Structure : Parsetree.structure ast_kind
| Signature : Parsetree.signature ast_kind

val read_ast : 'a ast_kind -> string -> 'a
val write_ast : 'a ast_kind -> string -> 'a -> unit

val file : tool_name:string -> all_ppx:string list -> string ->
  (Lexing.lexbuf -> 'a) -> 'a ast_kind -> 'a

val apply_rewriters
  :  ?restore:bool
  -> tool_name:string
  -> all_ppx:string list
  -> 'a ast_kind
  -> 'a
  -> 'a
  (** If [restore = true] (the default), cookies set by external
      rewriters will be kept for later calls. *)

val apply_rewriters_str
  :  ?restore:bool
  -> tool_name:string
  -> all_ppx:string list
  -> Parsetree.structure
  -> Parsetree.structure
val apply_rewriters_sig
  :  ?restore:bool
  -> tool_name:string
  -> all_ppx:string list
  -> Parsetree.signature
  -> Parsetree.signature

val report_error : formatter -> error -> unit

type parse_impl_fun
  =  tool_name:string
  -> preprocessor:string option
  -> all_ppx:string list
  -> string
  -> Parsetree.structure

val parse_implementation : parse_impl_fun

type parse_intf_fun
  =  tool_name:string
  -> preprocessor:string option
  -> all_ppx:string list
  -> string
  -> Parsetree.signature

val parse_interface : parse_intf_fun

(* [call_external_preprocessor sourcefile pp] *)
val call_external_preprocessor : string -> string -> string
val open_and_check_magic : string -> string -> in_channel * bool

module ImplementationHooks : Misc.HookSig with type t = Parsetree.structure
module InterfaceHooks : Misc.HookSig with type t = Parsetree.signature

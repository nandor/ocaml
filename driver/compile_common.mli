(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                            Gabriel Radanne                             *)
(*                                                                        *)
(*   Copyright 2018 Gabriel Radanne                                       *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Common compilation pipeline between bytecode and native. *)

(** {2 Initialization} *)

type info = {
  sourcefile : string;
  modulename : string;
  outputprefix : string;
  env : Env.t;
  ppf_dump : Format.formatter;
  tool_name : string;
}
(** Information needed to compile a file. *)

val init :
  Format.formatter ->
  init_path:bool ->
  tool_name:string ->
  sourcefile:string -> outputprefix:string -> info
(** [init ppf ~init_path ~tool_name ~sourcefile ~outputprefix] initializes
    the various global variables and returns an {!info}.
*)

(** {2 Interfaces} *)

type typecheck_intf_fun
  =  info
  -> Parsetree.signature
  -> Typedtree.signature

val emit_signature : info -> Parsetree.signature -> Typedtree.signature -> unit
(** [emit_signature info parsetree typedtree] emits the [.cmi] file
    containing the given signature.
*)

val interface :
  tool_name:string ->
  frontend:(Pparse.parse_intf_fun) option ->
  typing:typecheck_intf_fun option ->
  sourcefile:string ->
  outputprefix:string ->
  unit
(** The complete compilation pipeline for interfaces. *)

(** {2 Implementations} *)

type type_impl =
  { structure: Typedtree.structure
  ; coercion: Typedtree.module_coercion
  ; signature: Types.signature
  ; imports: Env.import_list
  }

type typecheck_impl_fun
  =  info
  -> Parsetree.structure
  -> type_impl

(** [typecheck_impl info parsetree] typechecks an implementation and returns
    the typedtree of the associated module, along with a coercion against
    its public interface.
*)

val implementation :
  tool_name:string ->
  native:bool ->
  frontend:(Pparse.parse_impl_fun) option ->
  typing:typecheck_impl_fun option ->
  backend:(info -> Typedtree.structure * Typedtree.module_coercion -> Env.import_list -> unit) ->
  sourcefile:string ->
  outputprefix:string ->
  unit
(** The complete compilation pipeline for implementations. *)

(** {2 Build artifacts} *)

val cmo : info -> string
val cmx : info -> string
val obj : info -> string
val annot : info -> string
(** Return the filename of some compiler build artifacts associated
    with the file being compiled.
*)

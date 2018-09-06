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

open Misc
open Compenv

type info = {
  sourcefile : string;
  modulename : string;
  outputprefix : string;
  env : Env.t;
  ppf_dump : Format.formatter;
  tool_name : string;
}


let cmx i = i.outputprefix ^ ".cmx"
let obj i = i.outputprefix ^ Config.ext_obj
let cmo i = i.outputprefix ^ ".cmo"
let annot i = i.outputprefix ^ ".annot"

let init ppf_dump ~init_path ~tool_name ~sourcefile ~outputprefix =
  Compmisc.init_path init_path;
  let modulename = module_of_filename sourcefile outputprefix in
  Env.set_unit_name modulename;
  let env = Compmisc.initial_env() in
  { modulename; outputprefix; env; sourcefile; ppf_dump; tool_name }


(** Compile a .mli file *)

type typecheck_intf_fun
  =  info
  -> Parsetree.signature
  -> Typedtree.signature

let typecheck_intf info ast =
  let tsg = ast |> Typemod.type_interface info.sourcefile info.env in
  let sg = tsg.Typedtree.sig_type in
  ignore (Includemod.signatures info.env sg sg);
  Typecore.force_delayed_checks ();
  Warnings.check_fatal ();
  tsg

let emit_signature info ast tsg =
  let sg, _ =
    let deprecated = Builtin_attributes.deprecated_of_sig ast in
    Env.save_signature ~deprecated tsg.Typedtree.sig_type
      info.modulename (info.outputprefix ^ ".cmi")
  in
  Typemod.save_signature info.modulename tsg
    info.outputprefix info.sourcefile info.env sg

let interface ~tool_name ~frontend ~typing ~sourcefile ~outputprefix =
  let frontend = Option.value frontend ~default:Pparse.parse_interface in
  let typing = Option.value typing ~default:typecheck_intf in
  let preprocessor = !Clflags.preprocessor in
  let all_ppx = !Clflags.all_ppx in
  Compmisc.with_ppf_dump ~fileprefix:(outputprefix ^ ".cmi") @@ fun ppf_dump ->
  Profile.record_call sourcefile @@ fun () ->
  let info =
    init ppf_dump ~init_path:false ~tool_name ~sourcefile ~outputprefix
  in
  let ast = frontend ~tool_name ~preprocessor ~all_ppx sourcefile
    |> print_if info.ppf_dump Clflags.dump_parsetree Printast.interface
    |> print_if info.ppf_dump Clflags.dump_source Pprintast.signature
  in
  let tsg = (Profile.(record typing) typing) info ast
    |> print_if info.ppf_dump Clflags.dump_typedtree Printtyped.interface
  in
  if !Clflags.print_types then
    Printtyp.wrap_printing_env ~error:false info.env (fun () ->
        Format.(fprintf std_formatter) "%a@."
          (Printtyp.printed_signature info.sourcefile)
          tsg.Typedtree.sig_type);
  if not !Clflags.print_types then begin
    emit_signature info ast tsg
  end


(** Frontend for a .ml file *)

type typecheck_impl_fun
  =  info
  -> Parsetree.structure
  -> Typedtree.structure * Typedtree.module_coercion * Types.signature * Env.import_list

let typecheck_impl type_impl i ast =
  let { sourcefile; outputprefix; modulename; env } = i in
  let sourceintf =
    Filename.remove_extension sourcefile ^ !Config.interface_suffix
  in
  let tst = Misc.try_finally (fun () ->
    let str, coercion, simple_sg, imports = type_impl i ast in
    let extra_import =
      if not !Clflags.print_types then begin
        let intf_exists = Sys.file_exists sourceintf in
        let cmi, imports =
          if not (intf_exists || !Clflags.dont_write_files) then
            (* Save the signature *)
            let deprecated = Builtin_attributes.deprecated_of_str ast in
            let cmi, import =
              Env.save_signature
                ~deprecated
                simple_sg
                modulename
                (outputprefix ^ ".cmi")
            in
            Some cmi, import :: imports
          else if intf_exists then
            (* This is a total hack to load crcs of the other intf. *)
            let intf_file =
              Misc.find_in_path_uncap !Config.load_path (modulename ^ ".cmi")
            in
            ignore (Env.read_signature modulename intf_file);

            (* Absolute total nightmare: we might load some extra imports *)
            let additional_imports = Env.imports () in
            let all_imports = List.append
              (imports |> List.map (fun (k, v) ->
                try (k, List.assoc k additional_imports)
                with Not_found -> (k, v)))
              (additional_imports |> List.filter (fun (k, _) ->
                not (List.exists (fun (k2, _) -> k == k2) imports)
              ))
            in
            None, all_imports |> List.sort (fun (ka, _) (kb, _) -> String.compare ka kb)
          else
            None, imports
        in
        Cmt_format.save_cmt
          (outputprefix ^ ".cmt")
          modulename
          (Cmt_format.Implementation str)
          (Some sourcefile)
          env
          cmi;
        imports
      end else
        imports
    in
    (str, coercion, extra_import)
  )
  ~always:(fun () -> Stypes.dump (Some (annot i)))
  ~exceptionally:(fun () ->
    let saved_types = Array.of_list (Cmt_format.get_saved_types ()) in
    Cmt_format.save_cmt
      (outputprefix ^ ".cmt")
      modulename
      (Cmt_format.Partial_implementation saved_types)
      (Some sourcefile)
      env
      None
  )
  in
  Warnings.check_fatal ();
  tst

let implementation ~tool_name ~native ~frontend ~typing ~backend ~sourcefile ~outputprefix =
  let frontend = Option.value frontend ~default:Pparse.parse_implementation in
  let preprocessor = !Clflags.preprocessor in
  let all_ppx = !Clflags.all_ppx in
  let suf, sufs = if native then ".cmx", [ cmx; obj ] else ".cmo", [ cmo ] in
  Compmisc.with_ppf_dump ~fileprefix:(outputprefix ^ suf) @@ fun ppf_dump ->
  let info =
    init ppf_dump ~init_path:native ~tool_name ~sourcefile ~outputprefix
  in
  Profile.record_call info.sourcefile @@ fun () ->
  let parsed = frontend ~tool_name ~preprocessor ~all_ppx info.sourcefile
    |> print_if info.ppf_dump Clflags.dump_parsetree Printast.implementation
    |> print_if info.ppf_dump Clflags.dump_source Pprintast.structure
  in
  let typing = Option.value typing ~default:(fun info ast ->
    let st, coer, sg =
      Typemod.type_implementation info.sourcefile info.modulename info.env ast
    in
    (st, coer, sg, Env.imports ())
  ) in
  let ts, coer, imports =
    (Profile.(record typing) (typecheck_impl typing)) info parsed
    |> print_if info.ppf_dump Clflags.dump_typedtree
        (fun ppf (ts, coer, _) -> Printtyped.implementation_with_coercion ppf (ts, coer))
  in
  if not !Clflags.print_types then begin
    let exceptionally () =
      List.iter (fun suf -> remove_file (suf info)) sufs;
    in
    Misc.try_finally ~exceptionally (fun () -> backend info (ts, coer) imports)
  end


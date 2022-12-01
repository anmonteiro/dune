open Import
open Dune_lang.Decoder

module Emit = struct
  type t =
    { loc : Loc.t
    ; alias : Alias.Name.t option
    ; module_system : Melange.Module_system.t
    ; entries : Ordered_set_lang.t
    ; libraries : Lib_dep.t list
    ; package : Package.t option
    ; preprocess : Preprocess.With_instrumentation.t Preprocess.Per_module.t
    ; preprocessor_deps : Dep_conf.t list
    ; promote : Rule.Promote.t option
    ; compile_flags : Ordered_set_lang.Unexpanded.t
    ; root_module : (Loc.t * Module_name.t) option
    ; javascript_extension : string
    }

  let decode_lib =
    let+ loc = loc
    and+ t =
      let allow_re_export = false in
      repeat (Lib_dep.decode ~allow_re_export)
    in
    let add kind name acc =
      match Lib_name.Map.find acc name with
      | None -> Lib_name.Map.set acc name kind
      | Some _present ->
        User_error.raise ~loc
          [ Pp.textf "library %S is present twice" (Lib_name.to_string name) ]
    in
    ignore
      (List.fold_left t ~init:Lib_name.Map.empty ~f:(fun acc x ->
           match x with
           | Lib_dep.Direct (_, s) -> add true s acc
           | Lib_dep.Re_export (_, name) ->
             User_error.raise ~loc
               [ Pp.textf
                   "library %S is using re_export, which is not supported for \
                    melange libraries"
                   (Lib_name.to_string name)
               ]
           | Select _ ->
             User_error.raise ~loc
               [ Pp.textf "select is not supported for melange libraries" ])
        : bool Lib_name.Map.t);
    t

  let decode =
    let extension_field name =
      let+ loc, extension =
        field name ~default:(Loc.none, "js") (located string)
      in
      if String.is_prefix ~prefix:"." extension then
        User_error.raise ~loc [ Pp.textf "extension must not start with '.'" ];
      "." ^ extension
    in
    fields
      (let+ loc = loc
       and+ alias = field_o "alias" Alias.Name.decode
       and+ module_system =
         field "module_system"
           (enum [ ("es6", Melange.Module_system.Es6); ("commonjs", CommonJs) ])
       and+ entries = Stanza_common.modules_field "entries"
       and+ libraries = field "libraries" decode_lib ~default:[]
       and+ package = field_o "package" Stanza_common.Pkg.decode
       and+ preprocess, preprocessor_deps = Stanza_common.preprocess_fields
       and+ promote = field_o "promote" Rule_mode_decoder.Promote.decode
       and+ loc_instrumentation, instrumentation = Stanza_common.instrumentation
       and+ compile_flags = Ordered_set_lang.Unexpanded.field "compile_flags"
       and+ root_module = field_o "root_module" Module_name.decode_loc
       and+ javascript_extension = extension_field "javascript_extension" in
       let preprocess =
         let init =
           let f libname = Preprocess.With_instrumentation.Ordinary libname in
           Module_name.Per_item.map preprocess ~f:(Preprocess.map ~f)
         in
         List.fold_left instrumentation ~init
           ~f:(fun accu ((backend, flags), deps) ->
             Preprocess.Per_module.add_instrumentation accu
               ~loc:loc_instrumentation ~flags ~deps backend)
       in
       { loc
       ; alias
       ; module_system
       ; entries
       ; libraries
       ; package
       ; preprocess
       ; preprocessor_deps
       ; promote
       ; compile_flags
       ; root_module
       ; javascript_extension
       })
end

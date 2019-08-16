
open Config
open Clflags
open Compenv

let process_implementation_file ppf name =
  let opref = output_prefix name in
  Optcompile.implementation ppf name opref;
  objfiles := (opref ^ ".cmx") :: !objfiles

let cmxa_present = ref false;;

let process_file ppf name =
  if Filename.check_suffix name ".ml"
  || Filename.check_suffix name ".mlt" then
    process_implementation_file ppf name
  else
    raise(Arg.Bad("don't know what to do with " ^ name))

let usage = "Usage: ocamlopt <options> <files>\nOptions are:"

let ppf = Format.err_formatter


let main () =
  native_code := true;
  let ppf = Format.err_formatter in
  try
    readenv ppf Before_args;
    Arg.parse (Arch.command_line_options @ Options.list) anonymous usage;
    readenv ppf Before_link;
    if
      List.length (List.filter (fun x -> !x)
                     [make_package; make_archive; shared;
                      compile_only; output_c_object]) > 1
    then
      fatal "Please specify at most one of -pack, -a, -shared, -c, -output-obj";
   if not !compile_only && !objfiles <> [] then begin
      let target =
        if !output_c_object then
          let s = extract_output !output_name in
          if (Filename.check_suffix s Config.ext_obj
            || Filename.check_suffix s Config.ext_dll)
          then s
          else
            fatal
              (Printf.sprintf
                 "The extension of the output file must be %s or %s"
                 Config.ext_obj Config.ext_dll)
        else
          default_output !output_name
      in
      Compmisc.init_path true;
      Asmlink.link ppf (get_objfiles ()) target;
      Warnings.check_fatal ();
    end;
    exit 0
  with x ->
      Location.report_exception ppf x;
      exit 2

let _ = main ()
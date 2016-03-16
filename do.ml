#!/usr/bin/env ocaml
(* installation etc *)

open List open Sys
let al = Array.to_list argv
let pl = print_endline

let exec c = pl ("\n#### "^c^" ####\n");
  if command c <> 0 then failwith "failed command"

let configure_qtest prefix = 
  exec "oasis setup";
  exec ("ocaml setup.ml -configure" ^ if prefix<>"" then " --prefix " ^ prefix else "")

let build_qtest prefix =
  configure_qtest prefix; exec "ocaml setup.ml -build"

let install_qtest prefix =
  build_qtest prefix;
  exec "ocamlfind remove QTest2Lib"; (* just in case *)
  exec "ocaml setup.ml -install"

let remove_qtest prefix =
  configure_qtest prefix;
  exec "ocaml setup.ml -uninstall"; (* not reliable ??? *)
  exec "ocamlfind remove QTest2Lib" (* you never know... *)
  
let main = match tl al with
| "qtest"::l ->
  (match l with
  | ["build"] -> build_qtest ""     | ["build";p] -> build_qtest p
  | ["install"] -> install_qtest "" | ["install";p] -> install_qtest p
  | ["remove"] -> remove_qtest ""   | ["remove";p] -> remove_qtest p
  | _ -> assert false
  )
| _ ->
  pl "usage: ./do.ml qtest {build,install,remove} [prefix]";
  failwith "bad command line"

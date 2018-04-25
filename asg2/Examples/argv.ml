(* $Id: argv.ml,v 346.4 2014-11-17 13:52:06-08 - - $ *)

(*
* Illustrate access to the command line.
*)

open Printf;;

let printarg i s = printf "argv.(%d) = %s\n" i s;;

printf "basename argv.(0) = %s\n" (Filename.basename Sys.argv.(0));;
printf "executable_name = %s\n" Sys.executable_name;;
Array.iteri printarg Sys.argv;;

exit 0;


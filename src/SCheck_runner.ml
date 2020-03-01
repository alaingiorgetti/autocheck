(********************************************************************)
(* Copyright (C) 2020 Alain Giorgetti and Clotilde Erard            *)
(* FEMTO-ST institute                                               *)
(********************************************************************)

(********************************************************************)
(*  This software is distributed under the terms of the GNU Lesser  *)
(*  General Public License version 2.1                              *)
(********************************************************************)

(* SCheck_runner.ml: Enumerative testing for OCaml, inspired by 
   QCheck_base_runner.ml from QCheck tool.
*)

open QCheck
open QCheck_base_runner
open SCheck

let print_verdict_inst e w =
  match e.Test.prt with
  | Some f -> f w
  | None -> "<no printer>"

let print_success ~colors out cell verd =
  Printf.fprintf out "\n+++ %a %s\n\n" (Color.pp_str_c ~colors `Green) "Success" (String.make 68 '+');
  Printf.fprintf out "Test %s succeeds (ran %d tests)\n%!"
    (Test.get_name cell)
    verd.Test.rank

let print_fail ~colors out cell verd wit =
  Printf.fprintf out "\n--- %a %s\n\n" (Color.pp_str_c ~colors `Red) "Failure" (String.make 68 '-');
  Printf.fprintf out "Test %s failed:\n\n%s\n%!"
    (Test.get_name cell)
    (print_verdict_inst verd wit)

let run_tests ?(colors=true) l : int =
  match l with
  | [] ->  Printf.fprintf stdout "No test:\n"; -1
  | test1 :: m ->
      let aux_print (Test.EnumTest cell) =
        let e = Test.exhaust cell in
        match e.Test.witness with
        | None -> 
            print_success ~colors stdout cell e
            ; -2
        | Some w -> print_fail ~colors stdout cell e w; 0
      in
        aux_print test1


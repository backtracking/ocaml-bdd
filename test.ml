(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Lesser General Public            *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(**************************************************************************)

open Bdd
open Prop

let p s =
  let lb = Lexing.from_string s in
  match Parser.file Lexer.token lb with
    | f :: _ -> f
    | _ -> assert false

module B = (val make 42)

let of_formula s =
  let nv, f = bdd_formula (p s) in assert (nv <= 42); B.build f
let test s = B.display (of_formula s)
let count s = B.count_sat (of_formula s)

let display b =
  B.print_to_dot b ~file:"test.dot";
  ignore (Sys.command "dot -Tpdf test.dot > test.pdf");
  ignore (Sys.command "evince test.pdf")

let test2 s = let b = of_formula s in display b

let r = test2 "(A -> B) -> (B -> A)"

let r = test "(a1<->a2)<->(a2<->a1)"
let r = test "A \\/ ~A"
let r = test "A -> ~~A"
let r = test "A -> A"
let r = test "((A -> B) -> A) -> A"
let r = test "(A -> B)-> (~B -> ~ A)"
let r = test "((A -> B) /\\ A) -> B"
let r = test "((A -> B) /\\ ~ B) -> ~ A"
let r = test "((A -> B) /\\ (B -> C)) -> (A -> C)"
let r = test "(A /\\ (B \\/ C)) -> ((A /\\ B) \\/ (A /\\ C))"
let r = test "((A /\\ B) \\/ (A /\\ C)) -> (A /\\ (B \\/ C))"
let r = test "(A \\/ (B /\\ C)) -> ((A \\/ B) /\\ (A \\/ C))"
let r = test "((A \\/ B) /\\ (A \\/ C)) -> (A \\/ (B /\\ C))"
let r = test "(~ A -> A) -> A "
let r = test "((P -> (Q /\\ R /\\ S)) /\\ ~S) -> ~P"
let r = test "(P /\\ Q) -> (Q /\\ P)"
let r = test "(A /\\ A) \\/ ~A"
let r = test "~~A <-> A"
let r = test "~(A /\\ B) <-> (~A \\/ ~B)"
let r = test "~(A \\/ B) <-> (~A /\\ ~ B)"
let r = test "(A \\/ (B /\\ C)) <-> ((A \\/ B) /\\ (A \\/ C))"
let r = test "(A /\\ (B \\/ C)) <-> ((A /\\ B) \\/ (A /\\ C))"

let r = test "((b <-> c) -> (a/\\b/\\c)) /\\
((c<->a)->(a/\\b/\\c)) /\\ ((a<->b)->(a/\\b/\\c)) -> (a/\\b/\\c)"

let r = test "~ ~(~p1 \\/ ~p2 \\/ ~p3 \\/ (p1 & p2 & p3))"

let de_bruijn_p_2 = test "
(((((p1 -> p2) & (p2 -> p1)) -> (p1 & (p2 & (p3 & (p4 & p5))))) & ((((p2 ->
p3) & (p3 -> p2)) -> (p1 & (p2 & (p3 & (p4 & p5))))) & ((((p3 -> p4) & (p4 ->
p3)) -> (p1 & (p2 & (p3 & (p4 & p5))))) & ((((p4 -> p5) & (p5 -> p4)) ->
(p1 & (p2 & (p3 & (p4 & p5))))) & (((p5 -> p1) & (p1 -> p5)) -> (p1 & (p2 &
(p3 & (p4 & p5))))))))) -> (p1 & (p2 & (p3 & (p4 & p5)))))"

let r = test2 "A -> (A -> ~ A)"
let r = test2 "A /\\ ~A"
let r = test2 "(A \\/ B) /\\ ~A /\\ ~B"
let r = test2 "(A -> B) -> (~A -> ~B)"
let r = test2 "(A -> B) -> (B -> A)"
let r = test2 "B -> (B /\\ A)"
let r = test2 "(A -> A) <-> A"

let () =
  let _, b = bdd_formula (Bench_prop.de_bruijn_n 5) in
  B.display (B.build b)

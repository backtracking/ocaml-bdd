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

module B = (val make 42)
let of_formula s =
  let nv, f = bdd_formula (Lexer.formula_of_string s) in
  assert (nv <= 42);
  B.build f

let valid s =
  assert (B.tautology (of_formula s))
let invalid s =
  assert (not (B.tautology (of_formula s)))

let () = valid "
((b <-> c) -> (a&b&c)) &
((c<->a)->(a&b&c)) & ((a<->b)->(a&b&c)) -> (a&b&c)"

let () = valid "~ ~(~p1 \\/ ~p2 \\/ ~p3 \\/ (p1 & p2 & p3))"

let () = valid "(a1<->a2)<->(a2<->a1)"

let () = valid " A \\/ ~A"
let () = valid " A -> ~~A"
let () = valid " A -> A"
let () = valid " ((A -> B) -> A) -> A"
let () = valid " (A -> B)-> (~B -> ~ A)"
let () = valid " ((A -> B) & A) -> B"
let () = valid " ((A -> B) & ~ B) -> ~ A"
let () = valid " ((A -> B) & (B -> C)) -> (A -> C)"
let () = valid " (A & (B \\/ C)) -> ((A & B) \\/ (A & C))"
let () = valid " ((A & B) \\/ (A & C)) -> (A & (B \\/ C))"
let () = valid " (A \\/ (B & C)) -> ((A \\/ B) & (A \\/ C))"
let () = valid " ((A \\/ B) & (A \\/ C)) -> (A \\/ (B & C))"
let () = valid " (~ A -> A) -> A "
let () = valid " ((P -> (Q & R & S)) & ~S) -> ~P"
let () = valid " (P & Q) -> (Q & P)"
let () = valid " (A & A) \\/ ~A"
let () = valid " ~~A <-> A"
let () = valid " ~(A & B) <-> (~A \\/ ~B)"
let () = valid " ~(A \\/ B) <-> (~A & ~ B)"
let () = valid " (A \\/ (B & C)) <-> ((A \\/ B) & (A \\/ C))"
let () = valid " (A & (B \\/ C)) <-> ((A & B) \\/ (A & C))"


let () = invalid " A -> (A -> ~ A)"
let () = invalid " A & ~A"
let () = invalid " (A \\/ B) & ~A & ~B"
let () = invalid " (A -> B) -> (~A -> ~B)"
let () = invalid " (A -> B) -> (B -> A)"
let () = invalid " B -> (B & A)"
let () = invalid " (A -> A) <-> A"

open Format

let print_count_sat s =
  let n = B.count_sat (of_formula s) in
  printf "count_sat(%s) = %Ld@." s n

let check_count_sat s n =
  let nv, f = bdd_formula (Lexer.formula_of_string s) in
  let module B = (val make nv) in
  assert (B.count_sat (B.build f) = n)

let () = check_count_sat "A" 1L
let () = check_count_sat "A \\/ B" 3L
let () = check_count_sat "A /\\ B" 1L
let () = check_count_sat "A /\\ (B \\/ C)" 3L

let () = check_count_sat "A \\/ ~A" 2L
let () = check_count_sat "(A \\/ ~A) /\\ (B \\/ ~B)" 4L

let () = print_endline "all tests successfully completed"


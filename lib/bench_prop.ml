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

open Prop

let pand p1 p2 = match p1, p2 with
  | Ptrue, p2 -> p2
  | p1, Ptrue -> p1
  | _ -> Pand (p1, p2)

let pands i j f =
  let rec mk k = if k > j then Ptrue else pand (f k) (mk (k+1)) in
  mk i

let piff p1 p2 = match p1, p2 with
  | Ptrue, p2 -> p2
  | p1, Ptrue -> p1
  | _ -> Piff (p1, p2)

let piffs i j f =
  let rec mk k = if k > j then Ptrue else piff (f k) (mk (k+1)) in
  mk i

let por p1 p2 = match p1, p2 with
  | Pfalse, p2 -> p2
  | p1, Pfalse -> p1
  | _ -> Por (p1, p2)

let pors i j f =
  let rec mk k = if k > j then Pfalse else por (f k) (mk (k+1)) in
  mk i

(* de bruijn *)

let var i = Pvar ("p" ^ string_of_int i)

let iff p1 p2 = Pand (Pimp (p1, p2), Pimp (p2, p1))

(**
de_bruijn_p(n) == LHS(2*n+1) -> RHS(2*n+1)
de_bruijn_n(n) == LHS(2*n) -> (p0 v RHS(2*n) v ~p0)

RHS(m) = &&_{i=1..m} p(i)
LHS(m) = &&_{i=1..m} ((p(i)<->p(i+1)) -> c(n))
where addition is computed modulo m.
***)

let lhs m =
  pands 1 m (fun i -> Pnot (iff (var i) (var (if i=m then 1 else i+1))))

let de_bruijn_p n = Pnot (lhs (2*n+1))
let de_bruijn_n n = Pnot (lhs (2*n))

(* pigeons

ph_p(n) =def left(n) -> right(n)

left(n) =def &&_{p=1..n+1} (vv_{j=1,..n} occ(i,j) )
right(n) =def vv_{h=1..n, p1=1..{n+1}, p2={p1+1}..{n+1}} s(i1,i2,j)
s(p1,p2,h) =def occ(p1,h) & occ(p2,h)

*)

let occ i j = Pvar ("occ_" ^ string_of_int i ^ "_" ^ string_of_int j)

let left n = pands 1 (n+1) (fun i -> pors 1 n (fun j -> occ i j))
let right n =
  pors 1 n (fun h ->
	      pors 1 (n+1) (fun p1 ->
			      pors (p1+1) (n+1) (fun p2 -> Pand (occ p1 h,
							    occ p2 h))))

let pigeon_p n = Pimp (left n, right n)

let pigeon_n _ = assert false

let equiv_p n =
  let f = ref (var n) in
  for i = 1 to n-1 do f := Piff (var (n-i), !f) done;
  for i = 1 to n do f := Piff (var (n+1-i), !f) done;
  !f

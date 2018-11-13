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

open Format

let n = try int_of_string Sys.argv.(1) with _ -> eprintf "queen N"; exit 1
include (val Bdd.make (n * n))

let fold_and i j f =
  let rec mk k = if k > j then one else mk_and (f k) (mk (k+1)) in
  mk i

let fold_or i j f =
  let rec mk k = if k > j then zero else mk_or (f k) (mk (k+1)) in
  mk i

let fold_for i j f =
  let rec fold k acc = if k > j then acc else fold (k+1) (f k acc) in
  fold i

(* 0..n-1 x 0..n-1 -> 1..n x n *)
let vars =
  Array.init n (fun i -> Array.init n (fun j -> mk_var (1 + n * i + j)))
let var i j = vars.(i).(j)

let constraints i j =
  let b1 =
    fold_and 0 (n-1)
      (fun l -> if l = j then one else mk_not (var i l))
  in
  let b2 =
    fold_and 0 (n-1)
      (fun k -> if k = i then one else mk_not (var k j))
  in
  let b3 =
    fold_and 0 (n-1)
      (fun k ->
	 let ll = j+k-i in
	 if ll >= 0 && ll < n && k <> i then mk_not (var k ll) else one)
  in
  let b4 =
    fold_and 0 (n-1)
      (fun k ->
	 let ll = j+i-k in
	 if ll >= 0 && ll < n && k <> i then mk_not (var k ll) else one)
  in
  mk_and b1 (mk_and b2 (mk_and b3 b4))

let bdd =
  fold_and 0 (n-1) (fun i -> fold_or 0 (n-1) (fun j -> var i j))

let bdd =
  fold_for 0 (n-1)
    (fun i acc ->
       fold_for 0 (n-1)
	 (fun j acc ->
	    mk_and acc (mk_imp (var i j) (constraints i j)))
	 acc)
    bdd

let () = printf "There are %Ld solutions@." (count_sat bdd)

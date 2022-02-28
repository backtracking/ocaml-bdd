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

let _ = if Array.length Sys.argv <> 2 then failwith ("usage: " ^ Sys.argv.(0) ^ " <size>")

let n = match int_of_string Sys.argv.(1) with
  | exception _ -> failwith "size should be a number"
  | n when n <= 0 -> failwith "size should be greater than 0"
  | n -> n

include (val Bdd.make ~size:60000 (n * n))

let fold_and i j f =
  let rec mk k = if k > j then one else mk_and (f k) (mk (k+1)) in
  mk i

let fold_or i j f =
  let rec mk k = if k > j then zero else mk_or (f k) (mk (k+1)) in
  mk i

let fold_for i j f =
  let rec fold k acc = if k > j then acc else fold (k+1) (f k acc) in
  fold i

let fold_for_rev i j f =
  let rec fold k acc = if k < j then acc else fold (k-1) (f k acc) in
  fold i

(* 0..n-1 x 0..n-1 -> 1..n x n *)
let vars =
  Array.init n (fun i -> Array.init n (fun j -> mk_var (1 + n * i + j)))
let var i j = vars.(i).(j)

let queens_s i j =
  let var i j = mk_var (1 + i * n + j) in
  fold_for_rev (n-1) 0 (fun row bdd ->
      if i = row then
        fold_for_rev (n-1) 0 (fun col bdd ->
            if j = col then
              mk_and bdd (var row col)
            else
              mk_and bdd (mk_not (var row col))
          ) bdd
      else
        let d = abs (i - row) in
        let bdd = if j + d < n then
                    mk_and bdd (mk_not (var row (j + d))) else bdd in
        let bdd = mk_and bdd (mk_not (var row j)) in
        if d <= j then
          mk_and bdd (mk_not (var row (j - d))) else bdd
    ) one
let queens_r i =
  fold_for 0 (n-1) (fun j bdd -> mk_or bdd (queens_s i j)) zero
let bdd =
  fold_for 0 (n-1) (fun i bdd -> mk_and bdd (queens_r i)) one

let () = printf "There are %d solutions@." (count_sat_int bdd)

let () = exit 0
let () =
  let print v (tl, e, sum, smallest, median, biggest) =
    printf "v=%d: size=%d #entries=%d sum=%d small/med/big=%d/%d/%d@."
      v tl e sum smallest median biggest in
  Array.iteri print (stats ())

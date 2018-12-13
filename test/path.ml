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

let _ =
  if Array.length Sys.argv <> 3 then failwith ("usage: " ^ Sys.argv.(0) ^ "<weight> <height>")

let w, h = match int_of_string Sys.argv.(1), int_of_string Sys.argv.(2) with
  | exception _ -> failwith "weight and height should be numbers"
  | w, h when w > 0 && h > 0 -> w, h
  | _ -> failwith "weight and height should be greater than 0"

let edges = ref []

let add_edge v1 v2 = edges := (v1, v2) :: !edges

let () =
  for i = 0 to w do for j = 0 to h do
    if i < w then add_edge (i,j) (i+1,j); (* right *)
    if j < h then add_edge (i,j) (i,j+1); (* down *)
  done done

let edges = Array.of_list !edges

let n_edges = Array.length edges
let () = printf "%d edges@." n_edges

include (val Bdd.make n_edges)

let adj = Hashtbl.create 17

let () =
  for i = 0 to n_edges - 1 do
    let e = i+1 in
    let v1,v2 = edges.(i) in
    Hashtbl.add adj v1 e;
    Hashtbl.add adj v2 e
  done

let rec iter_pairs f = function
  | [] | [_] ->
      ()
  | x :: l ->
      List.iter (f x) l;
      iter_pairs f l

let exactly_two_neighbors v =
  let adj_v = Hashtbl.find_all adj v in
  let b = ref zero in
  iter_pairs
    (fun e1 e2 -> (* e1 <> e2 *)
       (* we have edges e1 and e2 *)
       let b1 = ref (mk_and (mk_var e1) (mk_var e2)) in
       (* and no other edge for v *)
       List.iter
	 (fun e ->
	    if e <> e1 && e <> e2 then
	      b1 := mk_and !b1 (mk_not (mk_var e)))
	 adj_v;
       b := mk_or !b !b1)
    adj_v;
  !b

let () =
  printf "creating the bdd...@.";
  let bdd = ref one in
  for i = 0 to w do for j = 0 to h do
    printf "%d,%d @?" i j;
    let v = i,j in
    bdd := mk_and !bdd (exactly_two_neighbors v)
  done done;
  (* display !bdd; *)
  printf "counting...@.";
  printf "%Ld paths@." (count_sat !bdd);
(*   List.iter *)
(*     (fun ta -> *)
(*        List.iter *)
(* 	 (fun (e,b) -> *)
(* 	    if b then *)
(* 	      let (i1,j1),(i2,j2) = edges.(e-1) in *)
(* 	      printf "%d,%d -- %d,%d " i1 j1 i2 j2) *)
(* 	 ta; *)
(*        printf "---@." *)
(*     ) *)
(*     (all_sat !bdd); *)
  ()

(*
Local Variables:
compile-command: "make path.opt"
End:
*)

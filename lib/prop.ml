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

(** Propositional formulas with named variables
    (contrary to [Bdd.formula] where variables are integers). *)

type t =
  | Pvar of string
  | Pnot of t
  | Pand of t * t
  | Por  of t * t
  | Pimp of t * t
  | Piff of t * t
  | Ptrue
  | Pfalse

open Format

let print fmt p =
  let rec pr fmt = function
    | Pvar s -> fprintf fmt "%s" s
    | Pnot f -> fprintf fmt "(~%a)" pr f
    | Pand (f1, f2) -> fprintf fmt "(%a &@ %a)" pr f1 pr f2
    | Por (f1, f2) -> fprintf fmt "(%a v@ %a)" pr f1 pr f2
    | Pimp (f1, f2) -> fprintf fmt "(%a ->@ %a)" pr f1 pr f2
    | Piff (f1, f2) -> fprintf fmt "(%a <->@ %a)" pr f1 pr f2
    | Ptrue -> fprintf fmt "true"
    | Pfalse -> fprintf fmt "false"
  in
  fprintf fmt "@[%a@]" pr p

open Bdd

let bdd_formula f =
  let nbvar = ref 0 in
  let vars = Hashtbl.create 17 in
  let rec trans = function
    | Pvar s ->
	Fvar
	  (try Hashtbl.find vars s
	   with Not_found -> incr nbvar; Hashtbl.add vars s !nbvar; !nbvar)
    | Pnot f -> Fnot (trans f)
    | Pand (f1, f2) -> Fand (trans f1, trans f2)
    | Por (f1, f2) -> For (trans f1, trans f2)
    | Pimp (f1, f2) -> Fimp (trans f1, trans f2)
    | Piff (f1, f2) -> Fiff (trans f1, trans f2)
    | Ptrue -> Ftrue
    | Pfalse -> Ffalse
  in
  let f = trans f in
  !nbvar, f

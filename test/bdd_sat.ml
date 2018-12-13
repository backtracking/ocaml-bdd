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
open Prop
open Bdd

module Time = struct

  open Unix

  let utime f x =
    let u = (times()).tms_utime in
    let y = f x in
    let ut = (times()).tms_utime -. u in
    (y,ut)

  let print_utime f x =
    let (y,ut) = utime f x in
    printf "user time: %2.2f@." ut;
    y

end

let print_stats (module B : BDD) =
  Array.iter
    (fun (l,n,s,b1,b2,b3) ->
       printf "table length: %d / nb. entries: %d / sum of bucket length: %d@."
	 l n s;
       printf "smallest bucket: %d / median bucket: %d / biggest bucket: %d@."
	 b1 b2 b3)
    (B.stats ())

let nb = ref 0

let sat_unsat f =
  incr nb;
  let nbvar, f = bdd_formula f in
  let module B = (val make nbvar) in
  let b = B.build f in
  printf "%d: %s " !nb (if B.tautology b then "valid" else "invalid");
  print_stats (module B)

let check = Time.print_utime sat_unsat

let () =
  let lb = Lexing.from_channel stdin in
  let fl = Parser.file Lexer.token lb in
  List.iter check fl

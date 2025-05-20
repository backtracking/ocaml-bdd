
(* Build the BDD of a CNF formula in DIMACS format,
   then prints whether it is SAT (with a truth assignment) or UNSAT.

   Note: The DIMACS parser below is minimal and not robust at all
   (to a liberal use of spaces, in particular).
*)

let file =
  let file = ref None in
  Arg.parse [] (fun s -> file := Some s) "";
  match !file with
  | None -> Format.eprintf "%s dimacs-file@." Sys.argv.(0); exit 1
  | Some f -> f

let nv, cnf =
  let c = open_in file in
  let rec read_p () =
    let s = input_line c in
    if s = "" || s.[0] = 'c' then read_p () else
    Scanf.sscanf s "p cnf %d %d" (fun nv nc -> nv, nc) in
  let nv, nc = read_p () in
  let rec read_c cl b =
    let l = Scanf.bscanf b " %d" (fun i -> i) in
    if l = 0 then List.rev cl else read_c (l :: cl) b in
  let cnf = ref [] in
  for _ = 1 to nc do
    let b = Scanf.Scanning.from_string (input_line c) in
    cnf := read_c [] b :: !cnf
  done;
  nv, List.rev !cnf

let () =
  Format.printf "%d variables, %d clauses@." nv (List.length cnf)

open Bdd

module B = (val make nv)

let clause cl =
  let rec build bdd = function
    | [] -> bdd
    | v :: cl ->
        let lit = if v > 0 then B.mk_var v else B.mk_not (B.mk_var (-v)) in
        build (B.mk_or bdd lit) cl
  in
  build B.zero cl

let bdd =
  let rec build bdd = function
    | [] -> bdd
    | cl :: cll -> build (B.mk_and bdd (clause cl)) cll in
  build B.one cnf

open Format

let () = match B.any_sat bdd with
  | exception Not_found ->
      printf "UNSAT@."
  | vl ->
      printf "SAT@.";
      List.iter (fun (v, b) -> printf "%d " (if b then v else -v)) vl;
      printf "0@.";
      printf "#SAT = %d@." (B.count_sat_int bdd)

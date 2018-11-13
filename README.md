# ocaml-bdd
A simple BDD library for OCaml

This is a simple implementation of a BDD library for OCaml,
mostly for teaching and quick experiment purposes.

Files are:

- bdd.ml: the BDD library; bdd.mli should be self-explanatory

- check.ml: a quick check; "make check" compiles and runs it

- prop.mli, lexer.mll and parser.mly: propositional logic, with a parser,
  used to test the Bdd library; see check.ml for examples

- bench_prop.ml: various ways of generating valid propositional formulae;
  compiled as binary bench_prop.opt

- bdd_sat.ml: a propositional tautology checker using Bdd
  compiled as binary bdd_sat.opt

  these two binaries can be combined as follows:

	./bench_prop.opt -pigeon-p 7 | ./bdd_sat.opt
	1: valid user time: 0.60
	table length: 100003 / nb. entries: 2 / sum of bucket length: 20679
	smallest bucket: 0 / median bucket: 0 / biggest bucket: 3

- queen.ml: computes the number of solutions to the n-queens problem, using
  a propositional formula (this is not an efficient way to solve this problem,
  simply another way to test the Bdd library); compile with "make queen.opt"
  and run as

	./queen.opt 8
	There are 92 solutions

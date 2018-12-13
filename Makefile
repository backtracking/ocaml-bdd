build:
	dune build @all

doc:
	dune build @doc

clean:
	dune clean

test:
	dune runtest --force

install:
	dune build @install
	dune install

uninstall:
	dune uninstall

.PHONY: test

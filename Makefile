.PHONY: build test clean generate

build:
	dune build

test:
	dune runtest --force

clean:
	dune clean

generate:
	python3 tools/gen_tables.py > lib/idna_tables.ml

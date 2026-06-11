.PHONY: build test evidence conformance bench clean generate generate-64 generate-32

PYTHON_3_14 ?= uv run --python 3.14 python

build:
	dune build

test:
	dune runtest --force

evidence:
	dune build @test/evidence --force

conformance:
	dune build @test/conformance --force

bench:
	dune build @test/bench --force

clean:
	dune clean

generate: generate-64 generate-32

generate-64:
	$(PYTHON_3_14) tools/gen_tables.py --format 64 -o lib/idna-tables-64/idna_tables.ml

generate-32:
	$(PYTHON_3_14) tools/gen_tables.py --format 32 -o lib/idna-tables-32/idna_tables.ml

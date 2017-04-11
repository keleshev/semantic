.PHONY: test
test:
	ocamlopt -annot shim.ml parser.ml semver.ml test_parser.ml test_semver.ml -o test && ./test

.PHONY: clean
clean:
	rm -rf *.annot *.cmi *.cmx *.cmo *.o a.out test

.PHONY: test
test:
	ocamlopt -annot shim.ml parsing.ml semver.ml test_parsing.ml test_semver.ml -o test && ./test

.PHONY: clean
clean:
	rm -rf *.annot *.cmi *.cmx *.cmo *.o a.out test

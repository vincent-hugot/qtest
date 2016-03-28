
VERSION=$(shell grep Version: _oasis | awk '{ print $$2 }')

build:
	ocamlbuild -use-ocamlfind all.otarget

TO_INSTALL=src/META $(wildcard _build/src/qcheck.*) $(wildcard _build/src/QCheck*.cmi)

install:
	ocamlfind install qcheck $(TO_INSTALL)
	cp qtest.native $(BIN)/qtest

clean:
	ocamlbuild -clean

uninstall:
	ocamlfind remove qcheck
	rm $(BIN)/qtest

push_doc: doc
	scp -r qcheck.docdir/* cedeela.fr:~/simon/root/software/qcheck

man:
	mkdir -p man/man3/
	ocamlfind ocamldoc -I _build/ -man -d man/man3 src/QCheck*.ml*

.PHONY: man install_file tags update_next_tag

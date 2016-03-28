
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
	ocamlfind ocamldoc -I _build/ -man -d man/man3 qCheck.ml qCheck.mli

install_file: doc man
	@rm qcheck.install || true
	@echo 'doc: [' >> qcheck.install
	@for m in $(wildcard qcheck.docdir/*.html) ; do \
		echo "  \"?$${m}\"" >> qcheck.install; \
	done
	@echo ']' >> qcheck.install
	@echo 'man: [' >> qcheck.install
	@for m in $(wildcard man/man3/[A-Z]*.3o) ; do \
		echo "  \"?$${m}\"" >> qcheck.install; \
	done
	@echo ']' >> qcheck.install

VERSION=$(shell awk '/^Version:/ {print $$2}' _oasis)

update_next_tag:
	@echo "update version to $(VERSION)..."
	sed -i "s/NEXT_VERSION/$(VERSION)/g" *.ml *.mli
	sed -i "s/NEXT_RELEASE/$(VERSION)/g" *.ml *.mli

.PHONY: man install_file tags update_next_tag

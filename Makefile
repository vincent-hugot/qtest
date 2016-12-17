
build:
	ocamlbuild -use-ocamlfind all.otarget

install:
	cp qtest.native $(BIN)/qtest

clean:
	ocamlbuild -clean

uninstall:
	rm $(BIN)/qtest

doc: build
	@echo see readme

.PHONY: tags update_next_tag

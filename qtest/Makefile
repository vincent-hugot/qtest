OCAMLC     = ocamlfind ocamlc -thread
OCAMLOPT   = ocamlfind ocamlopt -thread
OCAMLDEP   = ocamlfind ocamldep
OCAMLDOC   = ocamlfind ocamldoc -html -colorize-code -thread -sort -stars
OCAMLLEX   = ocamllex
OCAMLYACC  = ocamlyacc
override OCAMLOPTFLAGS += $(INCS) -w Ael
override OCAMLFLAGS    += $(INCS) -w Ael

REQUIRES = oUnit str

# Common rules
.SUFFIXES: .mll .mly .ml .mli .cmo .cmi .cmx .opt .byte
.PHONY: opt clean install uninstall reinstall doc dep

all: qtest QTest2Lib.cma QTest2Lib.cmxa

install: qtest quickcheck.cmi runner.cmi QTest2Lib.cma META
	ocamlfind install qtest $^
uninstall:
	ocamlfind remove qtest
reinstall: uninstall install

.cmo.byte: $(ARCHIVE)
	$(OCAMLC)   -o $@ -package "$(REQUIRES)" $(ARCHIVE) -linkpkg $(OCAMLFLAGS) $<

.cmx.opt: $(XARCHIVE)
	$(OCAMLOPT) -o $@ -package "$(REQUIRES)" $(XARCHIVE) -linkpkg $(OCAMLOPTFLAGS) $<

.ml.cmo:
	$(OCAMLC) -package "$(REQUIRES)" $(OCAMLFLAGS) -c -impl $<

.mli.cmi:
	$(OCAMLC) -package "$(REQUIRES)" $(OCAMLFLAGS) -c -intf $<

.ml.cmx:
	$(OCAMLOPT) -package "$(REQUIRES)" $(OCAMLOPTFLAGS) -c -impl $<

.mll.ml:
	$(OCAMLLEX) $<

.mly.ml:
	$(OCAMLYACC) $<

.mly.mli:
	$(OCAMLYACC) $<

# Dependancies

qtest.ml: qtest.mll
qtest.cmo: qtest.ml core.cmo qparse.cmo
qtest.cmx: qtest.ml core.cmx qparse.cmx
core.cmo: misclex.cmo
core.cmx: misclex.cmx
qparse.cmo: qparse.cmi
qparse.cmx: qparse.cmi
quickcheck.cmo: quickcheck.cmi
quickcheck.cmx: quickcheck.cmi

# Main targets

QTest2Lib.cma: quickcheck.cmo runner.cmo
	$(OCAMLC) -a -o $@ -package "$(REQUIRES)" -custom -linkpkg $(OCAMLFLAGS) $^

QTest2Lib.cmxa: quickcheck.cmx runner.cmx
	$(OCAMLOPT) -a -o $@ -package "$(REQUIRES)" $(OCAMLOPTFLAGS) $^

qtest.byte: misclex.cmo core.cmo qparse.cmo qtest.cmo
	$(OCAMLC) -o $@ -package "$(REQUIRES)" -linkpkg $(OCAMLFLAGS) $^

qtest.opt: misclex.cmx core.cmx qparse.cmx qtest.cmx
	$(OCAMLOPT) -o $@ -package "$(REQUIRES)" -linkpkg $(OCAMLOPTFLAGS) $^

qtest: qtest.opt
	cp -lf $< $@

# Clean up

clean:
	rm -f *.cm[ioxa] *.cmxa *.a *.s *.o *.byte *.opt *.annot qparse.mli qtest.ml



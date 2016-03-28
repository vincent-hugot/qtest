set -e # stop on first error
# ./make.sh
rm -rf footest.ml _build
qtest -o footest.ml $@ extract foo.ml
ocamlbuild -cflags -warn-error,+26 -use-ocamlfind -pkg oUnit,qcheck footest.native
./footest.native

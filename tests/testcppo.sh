set -e # stop on first error
echo | cppo > /dev/null \
  || (echo "cppo is required to run this test"; exit 1)
cppo cppo.ml.cppo > cppo.ml
qtest extract cppo.ml -o cppo_test.ml
ocamlbuild -cflags -warn-error,+26 -use-ocamlfind -pkg oUnit,qcheck cppo_test.native
./cppo_test.native 2>&1 | grep cppo.test.ml >/dev/null \
  || { echo "test failed"; exit 1; } \
  && { rm -f cppo.ml cppo_test.ml; echo "test passed"; }



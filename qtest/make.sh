set -e
# rm -fv META
oasis setup
ocaml setup.ml -all

if [ "$1" = "install" ]
then
  echo INSTALLATION
  sudo ocamlfind remove QTest2Lib
  sudo ocaml setup.ml -install
fi
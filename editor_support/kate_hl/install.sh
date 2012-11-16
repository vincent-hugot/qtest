set -e
dir=~/.kde/share/apps/katepart/syntax/
mkdir -pv $dir
# cp -iv *.xml $dir
for i in ocaml{,yacc,lex}.xml
do
  ln -vs $(pwd)/$i $dir
done
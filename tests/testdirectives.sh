set -e # stop on first error
qtest extract directives.ml -o directives.ml.result
diff -u directives.ml.{result,reference} > /dev/null \
  || { echo "test failed"; exit 1; } \
  && { rm -f directives.ml.result; echo "test passed"; }

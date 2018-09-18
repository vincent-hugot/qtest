
build:
	@dune build @install

clean:
	@dune clean

doc:
	@dune doc

test: build
	cd tests && ./testfoo.sh || true
	cd tests && ./testcppo.sh || true
	cd tests && ./testdirectives.sh || true

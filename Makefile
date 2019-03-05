
build:
	@dune build @install

clean:
	@dune clean

test: build
	cd tests && ./testfoo.sh || true
	cd tests && ./testcppo.sh || true
	cd tests && ./testdirectives.sh || true

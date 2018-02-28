
build:
	jbuilder build @install

clean:
	jbuilder clean

doc:
	jbuider doc

test: build
	cd tests && ./testfoo.sh || true
	cd tests && ./testcppo.sh || true
	cd tests && ./testdirectives.sh || true

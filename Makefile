opts=--verbose --local

all:
	make deps
	make tests

install:
	jpm $(opts) install

clean:
	jpm $(opts) clean

tests:
	make install
	jpm $(opts) test

deps:
	jpm $(opts) deps

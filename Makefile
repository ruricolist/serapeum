asdf='(asdf:test-system :serapeum)'

all: reference.md

reference.md: $(wildcard *.lisp)
	ccl --load docs.lisp

.PHONY: test
test:
	ccl --batch --eval $(asdf) << /dev/null
	sbcl --non-interactive --eval $(asdf)

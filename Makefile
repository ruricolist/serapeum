asdf='(asdf:test-system :serapeum)'

all: REFERENCE.md

REFERENCE.md: $(wildcard *.lisp)
	ccl --load docs.lisp

REFERENCE.html: REFERENCE.md
	markdown $< > $@

.PHONY: test
test:
	ccl --batch --eval $(asdf) << /dev/null
	sbcl --non-interactive --eval $(asdf)

.PHONY: docs
docs: REFERENCE.md

asdf='(asdf:test-system :serapeum)'

all: reference.md

reference.md: $(wildcard *.lisp)
	ccl --load docs.lisp

reference.html: reference.md
	markdown $< > $@

.PHONY: test
test:
	ccl --batch --eval $(asdf) << /dev/null
	sbcl --non-interactive --eval $(asdf)

.PHONY: docs
docs: reference.md

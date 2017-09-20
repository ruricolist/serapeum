asdf='(asdf:test-system :serapeum)'

source_files = $(wildcard *.lisp)

all: REFERENCE.md

%.html: %.md
	pandoc $< -o $@

REFERENCE.md: $(source_files) $(wildcard $(source_files:.lisp=.md))
	ccl --load docs.lisp

.PHONY: test
test:
	ccl --batch --eval $(asdf) << /dev/null
	sbcl --non-interactive --eval $(asdf)

.PHONY: docs
docs: REFERENCE.md

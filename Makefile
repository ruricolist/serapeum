asdf='(uiop:quit (if (asdf:test-system :serapeum) 0 1))'

source_files = $(wildcard *.lisp)

all: REFERENCE.md

%.html: %.md
	pandoc $< -o $@

REFERENCE.md: $(source_files) $(wildcard $(source_files:.lisp=.md))
	ccl --load docs.lisp

.PHONY: test-sbcl test-ccl test-ecl test

test-sbcl:
	stdbuf -oL sbcl --non-interactive --eval $(asdf)

test-ccl:
	stdbuf -oL ccl --batch --eval $(asdf) << /dev/null

test-ecl:
	stdbuf -oL ecl -eval $(asdf) << /dev/null

test: test-sbcl test-ccl test-ecl

.PHONY: docs
docs: REFERENCE.md

asdf='(uiop:quit (if (asdf:test-system :serapeum) 0 1))'

source_files = $(wildcard *.lisp)

all: REFERENCE.md

%.html: %.md
	pandoc $< -o $@

REFERENCE.md: $(source_files) $(wildcard $(source_files:.lisp=.md))
	ccl --load docs.lisp

.PHONY: test-sbcl test-ccl test-ecl test

test-sbcl:
	sbcl --non-interactive --eval $(asdf) 2>&1 | sed -e 's/^/[SBCL] /'

test-ccl:
	ccl --batch --eval $(asdf) << /dev/null 2>&1 | sed -e 's/^/ [CCL] /'

test-ecl:
	ecl -eval $(asdf) << /dev/null 2>&1 | sed -e 's/^/ [ECL] /'

test: test-sbcl test-ccl test-ecl

.PHONY: docs
docs: REFERENCE.md

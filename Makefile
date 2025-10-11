asdf:='(uiop:quit (if (asdf:test-system :serapeum) 0 1))'

source_files := $(wildcard *.lisp)

CCL := ccl

DOC_PACKAGES := \
:serapeum/portability \
:serapeum/macro-tools \
:serapeum/types \
:serapeum/iter \
:serapeum/binding \
:serapeum/control-flow \
:serapeum/definitions \
:serapeum/defining-types \
:serapeum/conditions \
:serapeum/op \
:serapeum/box \
:serapeum \
:serapeum.exporting \
:serapeum.docs

all: REFERENCE.md

%.html: %.md
	pandoc $< -o $@

REFERENCE.md: $(source_files) $(wildcard $(source_files:.lisp=.md))
	CL_SOURCE_REGISTRY=`pwd`/ $(CCL) \
           --eval '(ql:quickload :serapeum/docs)' \
           --eval "(serapeum.docs:update-function-reference \"REFERENCE.md\" :serapeum (list $(DOC_PACKAGES)))" \
           --eval '(uiop:quit)'

.PHONY: test-sbcl test-ccl test-ecl test

test-sbcl:
	sbcl --non-interactive --eval $(asdf) 2>&1 | sed -e 's/^/[SBCL] /'

test-ccl:
	ccl --batch --eval $(asdf) << /dev/null 2>&1 | sed -e 's/^/ [CCL] /'

test-ecl:
	ecl -eval $(asdf) << /dev/null 2>&1 | sed -e 's/^/ [ECL] /'

test: test-sbcl test-ccl test-ecl
	echo "Test suites passed."; beep

.PHONY: docs
docs: REFERENCE.md

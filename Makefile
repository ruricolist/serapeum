all: reference.md

reference.md: $(wildcard *.lisp)
	ccl --load docs.lisp


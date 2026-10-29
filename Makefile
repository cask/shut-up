CASK ?= cask
EMACS ?= emacs

all: test

test: compile
	${CASK} exec ert-runner

compile:
	${CASK} emacs -Q --batch -L . --eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile shut-up.el

clean:
	rm -f shut-up.elc

.PHONY:	all test compile clean

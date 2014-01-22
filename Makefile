CASK ?= cask
EMACS ?= emacs

all: test

test:
	${CASK} exec ert-runner

.PHONY:	all test

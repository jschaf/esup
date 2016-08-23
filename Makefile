test:
	cask exec ert-runner

byte-compile: elpa
	emacs -Q -L . -batch -f batch-byte-compile esup.el esup-child.el

clean:
	rm -f *.elc

.PHONY: all test clean byte-compile

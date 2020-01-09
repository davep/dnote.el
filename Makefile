dnote.elc: dnote.el
	emacs -batch -f batch-byte-compile $<

clean:
	rm -f *.elc

### Makefile ends here

EMACS ?= emacs

BATCH = $(EMACS) --batch -L .
ELENV = -l test/run-test

.PHONY: elpa build build-strict byte-compile init-load clean-elpa clean-elc simple-start

elpa:
	mkdir -p elpa
	$(BATCH) -l elisp/melpa.el -l site-start.d/00_init-package.el --eval \
		'(my-install-package)'

build:
	$(BATCH) -l test/run-test --eval \
		'(batch-byte-compile)' site-start.d/*.el

init-load:
	$(BATCH)  -l test/run-test --eval '(load (concat default-directory "init.el"))'

build-strict:
	-$(BATCH) -l test/run-test
	-$(BATCH) -l test/run-test \
	--eval '(byte-compile-disable-warning \'cl-functions)' \
	--eval 	'(progn \
		(message default-directory) \
		(message (concat default-directory "test/run-test.el")) \
		(load (concat user-emacs-directory "test/run-test.el")) \
		(setq byte-compile-error-on-warn t) \
		(batch-byte-compile))' ~/.emacs.d/site-start.d/*.el

byte-compile:
	$(BATCH) -l test/run-test -l site-start.d/01_init-global.el --eval \
		'(my-byte-compile-func)'

clean-config-elc:
	rm -f init.elc
	find site-start.d -name "*.elc" | xargs --no-run-if-empty rm

clean-elc:
	rm init.elc
	find site-start.d -name "*.elc" | xargs rm
	find elisp -name "*.elc" | xargs rm
	find plugins -name "*.elc" | xargs rm

clean-elpa:
	rm -r elpa

simple-start:
	./test-startup.sh

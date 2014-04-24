EMACS ?= "${HOME}/workspace/emacs/emacs-24.3/nextstep/Emacs.app/Contents/MacOS/Emacs"

BATCH = $(EMACS) --batch -L .
ELENV = -l tests/run-test

.PHONY: elpa build build-strict byte-compile init-load clean-elpa clean-elc

elpa:
	@echo "  start elpa"
	mkdir -p elpa
	$(BATCH) -l elisp/melpa.el -l site-start.d/00_init-package.el --eval \
		'(my-install-package)'
	@echo "  end elpa"

build:
	@echo "  start build"
	$(BATCH) -l tests/run-test --eval \
		'(batch-byte-compile)' site-start.d/*.el
	@echo "  end build"

init-load:
	$(BATCH)  -l tests/run-test --eval '(load (concat default-directory "init.el"))'
# (mapcar #\'(lambda (x) (print x)) \'load-path) \

build-strict:
	-$(BATCH) -l tests/run-test
	-$(BATCH) -l tests/run-test \
	--eval '(byte-compile-disable-warning \'cl-functions)' \
	--eval 	'(progn \
		(message default-directory) \
		(message (concat default-directory "tests/run-test.el")) \
		(load (concat user-emacs-directory "tests/run-test.el")) \
		(setq byte-compile-error-on-warn t) \
		(batch-byte-compile))' ~/.emacs.d/site-start.d/*.el

byte-compile:
	$(BATCH) -l tests/run-test -l site-start.d/01_init-global.el --eval \
		'(my-byte-compile-func)'

clean-elc:
	rm -rf site-start.d/*.elc

clean-elpa:
	rm -r elpa


test-modes:
	$(BATCH) $(ELENV) --eval '(message "test")'

# --eval "(mapcar #'(lambda (x) (message x)) load-path)"
test-modes1:
	$(BATCH) $(ELENV) --eval "(message \"test\")" \
		--script tests/run-test.el tests/run-mode-test.el

EMACS ?= "${HOME}/workspace/emacs/emacs-24.3/nextstep/Emacs.app/Contents/MacOS/Emacs"

BATCH = $(EMACS) --batch -L .

.PHONY: elpa build build-strict init-load clean-elpa

elpa:
	@echo "  start elpa"
	mkdir -p elpa
	$(BATCH) -l elisp/melpa.el -l site-start.d/00_init-package.el --eval \
		'(progn \
		(load "~/.emacs.d/site-start.d/00_init-package.el") \
		(my-install-package))'
	@echo "  end elpa"

build:
	@echo "  start build"
	$(BATCH) -l tests/run-test --eval \
		'(batch-byte-compile)' site-start.d/*.el
	@echo "  end build"

init-load:build
	$(BATCH)  -l tests/run-test --eval '(load (concat default-directory "init.el"))'
# (mapcar #\'(lambda (x) (print x)) \'load-path) \

build-strict:
	-$(BATCH) -l tests/run-test
	-$(BATCH) -l tests/run-test \
	--eval "(byte-compile-disable-warning 'cl-functions)" \
	--eval 	'(progn \
		(message default-directory) \
		(message (concat default-directory "tests/run-test.el")) \
		(load (concat user-emacs-directory "tests/run-test.el")) \
		(setq byte-compile-error-on-warn t) \
		(batch-byte-compile))' ~/.emacs.d/site-start.d/*.el

# clean:
# 	rm -rf site-start.d/*.elc

clean-elpa:
	rm -r elpa

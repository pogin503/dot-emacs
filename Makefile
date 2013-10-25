EMACS ?= "${HOME}/workspace/emacs/emacs-24.3/nextstep/Emacs.app/Contents/MacOS/Emacs"

elpa:
	@echo "  start elpa"
	mkdir elpa
	${EMACS} --batch -L . -l elisp/melpa.el -l site-start.d/00_init-package.el
	@echo "  end elpa"

build:
	@echo "  start build"
	-$(EMACS) --batch -L . -l tests/run-test -f add-to-load-path --eval \
		'(progn \
		(add-to-load-path "site-start.d" "elpa" "elisp" "plugins" "etc") \
		(batch-byte-compile))' ~/.emacs.d/site-start.d/*.el
	@echo "  end build"

build-strict:
	-$(EMACS) --batch -L . -l tests/run-test --eval \
		'(progn \
		(add-to-load-path "site-start.d" "elpa" "elisp" "plugins" "etc") \
		(setq byte-compile-error-on-warn t) \
		(batch-byte-compile))' ~/.emacs.d/site-start.d/*.el

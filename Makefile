EMACS ?= "${HOME}/workspace/emacs/emacs-24.3/nextstep/Emacs.app/Contents/MacOS/Emacs"

build:
	$(EMACS) --batch -L . -l tests/run-test -f add-to-load-path --eval \
		'(progn \
		(add-to-load-path "site-start.d" "elpa" "elisp" "plugins") \
		(batch-byte-compile))' ~/.emacs.d/site-start.d/*.el

build-strict:
	$(EMACS) --batch -L . -l tests/run-test --eval \
		'(progn \
		(setq byte-compile-error-on-warn t) \
		(batch-byte-compile))' ~/.emacs.d/site-start.d/*.el

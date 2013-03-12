;; Let's run 8 checks at once instead.
;; (setq flymake-max-parallel-syntax-checks 8)
;; (require 'flymake)
;; I don't want no steekin' limits.
;; (setq flymake-max-parallel-syntax-checks nil)

;; Yes, I want my copies in the same dir as the original.
;; (setq flymake-run-in-place t)

;; Nope, I want my copies in the system temp dir.
;; (setq flymake-run-in-place nil)
;; This lets me say where my temp dir is.
;; (setq temporary-file-directory nil)
;; (setq temporary-file-directory nil)
;; (load "~/.emacs.d/plugins/flymake/flymake.elc")

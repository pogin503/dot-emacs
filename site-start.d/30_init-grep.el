;;; 30_init-grep --- 30_init-grep
;; This program is free software
;;; Commentary:
;;; Code:
(setq grep-host-defaults-alist nil)
(setq grep-template "lgrep <C> -n <R> <F> <N>")
(setq grep-find-template "find . <X> -type f <F> -print0 | xargs -0 -e lgrep <C> -n <R> <N>")

(defvar quote-argument-for-windows-p t "Enables `shell-quote-argument' workaround for windows.")
(defadvice shell-quote-argument (around shell-quote-argument-for-win activate)
  "Workaround for windows."
  (if quote-argument-for-windows-p
      (let ((argument (ad-get-arg 0)))
	(setq argument (replace-regexp-in-string "\\\\" "\\\\" argument nil t))
	(setq argument (replace-regexp-in-string "'" "'\\''" argument nil t))
	(setq ad-return-value (concat "'" argument "'")))
    ad-do-it))

(use-package ag
  :ensure t)

(provide '30_init-grep)
;;; 30_init-grep ends here

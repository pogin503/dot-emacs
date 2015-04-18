;;; 50_init-coq-mode --- 50_init-coq-mode
;; This program is free software
;;; Commentary:
;;; Code:

(require '00_init-hanbetu)
(require '30_init-anything)

(require 'f)
(when (f-exists? "/usr/local/opt/coq/lib/emacs/site-lisp")
  (progn
	(setq auto-mode-alist (cons '("\\.v$" . coq-mode) auto-mode-alist))
	(autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)
	(custom-set-variables
	 '(coq-prog-name "/usr/local/bin/coqtop.opt"))
	))

;; Windows Setting
(if run-windows
    (if run-windows-x64
		(add-to-list 'exec-path "C:/Program Files (x86)/Coq/bin")
      (add-to-list 'exec-path "C:/Program Files/Coq/bin"))
  (load-file
   (concat dropbox-directory
           "/eworkspace/ProofGeneral-4.2/ProofGeneral-4.2/generic/proof-site.el"))
  (load-file
   (concat dropbox-directory
           "/eworkspace/ssreflect-1.4/pg-ssr.el")))

(provide '50_init-coq-mode)
;;; 50_init-coq-mode ends here

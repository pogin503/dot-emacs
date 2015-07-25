;;; 40_init-auto-complete --- auto-complete conf
;;; Commentary:
;; @see http://cx4a.org/software/auto-complete/manual.ja.html
;;; Code:

(use-package auto-complete
  :config
  (progn
	(require 'auto-complete-config)

	(add-to-list 'ac-dictionary-directories
				 (concat user-emacs-directory "plugins/auto-complete/ac-dict/"))
	(setq ac-comphist-file (concat user-emacs-directory  "etc/ac-comphist.dat"))
	(ac-config-default)
	;; (global-auto-complete-mode t)
	(setq-default ac-use-comphist nil)
	(setq-default ac-auto-start 4
				  ac-auto-show-menu 1.0)
	(define-key ac-mode-map (kbd "M-i") 'auto-complete)
	(define-key ac-mode-map (kbd "H-i") 'auto-complete)
	(define-key ac-completing-map "\t" 'ac-complete)

	;; Enterで補完をしないようにする
	(define-key ac-completing-map "\r" nil)

	(dolist (hook (list
				   'html-mode-hook
				   'sgml-mode-hook
				   'nxml-mode-hook
				   ))
	  (add-hook hook 'auto-complete-mode))

	(add-to-list 'ac-modes 'haskell-mode)

	;; coq
	(defun my-ac-coq-mode ()
	  "Set coq ac-source."
	  (setq ac-sources '(ac-source-words-in-same-mode-buffers
						 ac-source-dictionary)))
	))

(provide '40_init-auto-complete)
;;; 40_init-auto-complete ends here

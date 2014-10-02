;;; 40_init-auto-complete --- auto-complete conf
;;; Commentary:
;; @see http://cx4a.org/software/auto-complete/manual.ja.html
;;; Code:

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/plugins/auto-complete/ac-dict/")
(ac-config-default)
;; (global-auto-complete-mode t)
(setq-default ac-use-comphist nil)
(setq-default ac-auto-start 4
              ac-auto-show-menu 1.0)

(define-key ac-mode-map (kbd "M-i") 'auto-complete)
(define-key ac-mode-map (kbd "H-i") 'auto-complete)
(define-key ac-completing-map "\t" 'ac-complete)
(define-key ac-completing-map "\r" nil)
(ac-flyspell-workaround)

(dolist (hook (list
               'html-mode-hook
               'sgml-mode-hook
               'nxml-mode-hook
               ))
  (add-hook hook 'auto-complete-mode))

(setq ac-comphist-file (concat user-emacs-directory  "cache/auto-complete/ac-comphist.dat"))

(provide '40_init-auto-complete)
;;; 40_init-auto-complete ends here

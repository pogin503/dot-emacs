;;; 40_init-auto-complete --- auto-complete conf
;;; Commentary:
;;; Code:
;;auto-complete******************************
;; (add-to-list 'load-path "~/.emacs.d/plugins/auto-complete")
;; (req auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/plugins/auto-complete/ac-dict/")
(ac-config-default)
(global-auto-complete-mode t)
(setq-default ac-use-comphist t)
(define-key ac-mode-map (kbd "M-i") 'auto-complete)
(define-key ac-completing-map "\t" 'ac-complete)
(define-key ac-completing-map "\r" nil)
(ac-flyspell-workaround)

(dolist (hook (list
               'html-mode-hook
               'sgml-mode-hook
               'nxml-mode-hook
               ))
  (add-hook hook 'auto-complete-mode))
(provide '40_init-auto-complete)
;;; 40_init-auto-complete ends here

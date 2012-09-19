;;auto-complete******************************
(add-to-list 'load-path "~/.emacs.d/plugins/auto-complete")
;; (req auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/plugins/auto-complete/ac-dict/")
(ac-config-default)
(global-auto-complete-mode t)
(setq ac-use-comphist t)
(define-key ac-mode-map (kbd "M-i") 'auto-complete)
(ac-flyspell-workaround)

(dolist (hook (list
               'html-mode-hook
               'sgml-mode-hook
               'nxml-mode-hook
               ))
  (add-hook hook 'auto-complete-mode))

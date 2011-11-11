;;auto-complete******************************
(add-to-list 'load-path "~/.emacs.d/auto-complete/")

(req auto-complete)
(global-auto-complete-mode t)

;;@see http://stackoverflow.com/questions/4281583/i-get-a-error-when-i-try-install-auto-complete-in-emacs
;; (eval-after-load 'auto-complete-config
;;     '(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/ac-dict"))
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/ac-dict/")
(req auto-complete-config)
(ac-config-default)
(setq ac-use-comphist t)
(define-key ac-mode-map (kbd "M-i") 'auto-complete)

;;end

(ac-flyspell-workaround)

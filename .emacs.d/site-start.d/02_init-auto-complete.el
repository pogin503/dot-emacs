;;auto-complete******************************
(add-to-list 'load-path "~/.emacs.d/auto-complete/")    ; load-path‚É‚·‚Å‚Éİ’è‚³‚ê‚Ä‚¢‚éê‡‚Í•\‚ê‚Ü‚¹‚ñ
;;;;
;;@see http://stackoverflow.com/questions/4281583/i-get-a-error-when-i-try-install-auto-complete-in-emacs
(eval-after-load 'auto-complete-config
    '(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/ac-dict"))
;;;;

(require 'auto-complete)
(global-auto-complete-mode t)
(require 'auto-complete-config)
(ac-config-default)

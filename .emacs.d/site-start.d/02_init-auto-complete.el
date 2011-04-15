;;auto-complete******************************
(add-to-list 'load-path "~/.emacs.d/auto-complete/")    ; load-pathにすでに設定されている場合は表れません
;;;;
;;@see http://stackoverflow.com/questions/4281583/i-get-a-error-when-i-try-install-auto-complete-in-emacs
(eval-after-load 'auto-complete-config
    '(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/ac-dict"))
;;;;

(require 'auto-complete)
(global-auto-complete-mode t)
(require 'auto-complete-config)
(ac-config-default)

;;auto-complete******************************
(add-to-list 'load-path "~/.emacs.d/plugins/auto-complete")
(add-to-list 'ac-dictionary-directories "~/.emacs.d/plugins/auto-complete/ac-dict")
(req auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)
(ac-flyspell-workaround)
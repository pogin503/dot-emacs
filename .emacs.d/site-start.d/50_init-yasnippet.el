;;; Code:

(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet-0.6.1c")

(require 'yasnippet)
(require 'anything-c-yasnippet)
(setq anything-c-yas-space-match-any-greedy t)
(global-set-key (kbd "C-c y") 'anything-c-yas-complete)
(yas/initialize)
(setq yas/root-directory '("~/.emacs.d/etc/mysnippets"
			   "~/.emacs.d/plugins/yasnippet-0.6.1c/snippets"))
(mapc 'yas/load-directory  yas/root-directory)

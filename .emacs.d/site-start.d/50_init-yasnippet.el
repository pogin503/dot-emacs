;;; Code:
(require 'yasnippet)

(add-to-list 'load-path
	     "~/.emacs.d/plugins/yasnippet-0.6.1c")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/.emacs.d/plugins/yasnippet-0.6.1c/snippets")


;;reference from sakito's init-yasnippet
;;@see 

;; http://svn.coderepos.org/share/lang/elisp/anything-c-yasnippet/anything-c-yasnippet.el
(require 'anything-c-yasnippet)
(setq anything-c-yas-space-match-any-greedy t)
(global-set-key (kbd "C-c y") 'anything-c-yas-complete)

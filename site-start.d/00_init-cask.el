;;; 00_init-cask.el --- for cask setting
;;; Commentary:
;;; Code:

;; .cask
;; cask init
(add-to-list 'auto-mode-alist '("Cask$" . emacs-lisp-mode))
(require 'cask "~/.cask/cask.el")
(cask-initialize)
;;; 00_init-cask.el ends here

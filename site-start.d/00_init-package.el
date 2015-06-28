;;; 00_init-package.el --- package conf
;;; Commentary:
;;; Code:
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)

(package-initialize)

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(add-to-list 'auto-mode-alist '("Cask$" . emacs-lisp-mode))

(require 'use-package)

(use-package pallet
  :config
  (pallet-mode +1))

(use-package ag
  :ensure ag)

(use-package elisp-slime-nav
  :ensure elisp-slime-nav
  :config
  (progn
	(dolist (hook '(emacs-lisp-mode-hook
                    ielm-mode-hook
                    lisp-interaction-mode-hook))
	  (add-hook hook 'turn-on-elisp-slime-nav-mode))))

(use-package ido
  :config
  (progn
	(ido-mode +1)
	(custom-set-variables
	 '(ido-enable-flex-matching t)
	 '(ido-use-filename-at-point 'guess)
	 '(ido-everywhere t)
	 '(ido-use-faces nil)
	 '(ido-ignore-extensions t))
	))

(use-package multiple-cursors
  :bind (("C-c C-S-c" . mc/edit-lines)
		 ("C->" . mc/mark-next-like-this)
		 ("C-<" . mc/mark-previous-like-this)
		 ("C-c C-<"  . mc/mark-all-like-this)
		 ("C-*"  . mc/mark-all-like-this)))

(use-package popwin
  :config
  (progn
	;;dired
	(push '(dired-mode :position top) popwin:special-display-config)

	;; Apropos
	(push '("*slime-apropos*") popwin:special-display-config)
	;; Macroexpand
	(push '("*slime-macroexpansion*") popwin:special-display-config)
	;; Help
	(push '("*slime-description*") popwin:special-display-config)
	;; Compilation
	(push '("*slime-compilation*" :noselect t) popwin:special-display-config)
	;; Cross-reference
	(push '("*slime-xref*") popwin:special-display-config)
	;; Debugger
	(push '(sldb-mode :stick t) popwin:special-display-config)
	;; REPL
	(push '(slime-repl-mode) popwin:special-display-config)
	;; Connections
	(push '(slime-connection-list-mode) popwin:special-display-config)

	;;@see http://d.hatena.ne.jp/sokutou-metsu/20110205/1296915272
	(push '(" *auto-async-byte-compile*" :height 14 :position bottom :noselect t) popwin:special-display-config)
	(push '("*VC-log*" :height 10 :position bottom) popwin:special-display-config)
	;;Compile-Log
	(push '("*Compile-Log*" :height 10 :noselect t) popwin:special-display-config)
	(push '("*Process List*" :stick t) popwin:special-display-config)
	(push '("*sdic*" :noselect t)  popwin:special-display-config)
	(push '("*init log*" :stick t) popwin:special-display-config)
	(push '("\\*magit.*" :stick t :regexp t :height 25) popwin:special-display-config)
	(push '("COMMIT-EDITMSG" :height 15) popwin:special-display-config)
	(push '("*compilation*" :regexp t) popwin:special-display-config)
	(push '("*ert*" :regexp t) popwin:special-display-config)
	(push '("*Codic Result*" :height 15) popwin:special-display-config)
    )
  (setq popwin:close-popup-window-timer-interval 0.7)
  ;; (popwin-mode 1)
  (setq display-buffer-function 'popwin:display-buffer)
  )

(use-package volatile-highlights
  :config
  (volatile-highlights-mode t))

(use-package buffer-move
  :config
  (progn
	(global-set-key (kbd "C-c <left>")  'buf-move-left)
	(global-set-key (kbd "C-c <down>")  'buf-move-down)
	(global-set-key (kbd "C-c <up>")    'buf-move-up)
	(global-set-key (kbd "C-c <right>") 'buf-move-right)
	))

(use-package ats-mode
  :mode ("\\.dats\\'" . ats-mode)
  :commands (ats-mode))

(use-package peep-dired
  :config
  (define-key dired-mode-map "\C-xx" 'peep-dired)
  (define-key peep-dired-mode-map "\C-xx" 'peep-dired)
  )

(use-package sudo-ext)

(use-package zlc
  :config
  (zlc-mode 1))

(use-package open-junk-file)

(use-package anzu
  :bind  ("M-%" . anzu-query-replace-regexp)
  :config
  (global-anzu-mode +1))

;;; 00_init-package.el ends here

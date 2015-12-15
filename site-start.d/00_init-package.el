;;; 00_init-package.el --- package conf
;;; Commentary:
;;; Code:
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(add-to-list 'package-archives
             '("melpa-stable" . "https://melpa-stable.milkbox.net/packages/") t)

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
  (ido-mode +1)
  (custom-set-variables
   '(ido-enable-flex-matching t)
   '(ido-use-filename-at-point 'guess)
   '(ido-everywhere t)
   '(ido-use-faces nil)
   '(ido-save-directory-list-file (locate-user-emacs-file "cache/ido.last"))
   '(ido-ignore-extensions t)))

(use-package multiple-cursors
  :bind (("C-c C-S-c" . mc/edit-lines)
		 ("C->" . mc/mark-next-like-this)
		 ("C-<" . mc/mark-previous-like-this)
		 ("C-c C-<"  . mc/mark-all-like-this)
		 ("C-*"  . mc/mark-all-like-this))
  :config
  (setq mc/list-file (locate-user-emacs-file "cache/.mc-lists.el")))


(use-package popwin
  :config
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
  (setq popwin:close-popup-window-timer-interval 0.7)
  ;; (popwin-mode 1)
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
	(global-set-key (kbd "C-c <right>") 'buf-move-right)))

(use-package ats-mode
  :mode ("\\.dats\\'" . ats-mode)
  :commands (ats-mode))

(use-package peep-dired
  :config
  (define-key dired-mode-map "\C-xx" 'peep-dired)
  (define-key peep-dired-mode-map "\C-xx" 'peep-dired))

(use-package sudo-ext)

(use-package zlc
  :config
  (zlc-mode 1))

(use-package open-junk-file)

(use-package anzu
  :bind  ("M-%" . anzu-query-replace-regexp)
  :config
  (global-anzu-mode +1))

(use-package helm-swoop
  :config
  ;; When doing isearch, hand the word over to helm-swoop
  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
  ;; From helm-swoop to helm-multi-swoop-all
  (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package pretty-lambdada
  :config
  (pretty-lambda-for-modes))

(use-package sequential-command-config
  :config
  (sequential-command-setup-keys))

(use-package diminish
  :config
  (defmacro safe-diminish (file mode &optional new-name)
    `(with-eval-after-load ,file
       (diminish ,mode ,new-name)))

  (safe-diminish "anzu" 'anzu-mode)
  (safe-diminish "auto-complete" 'auto-complete-mode)
  (safe-diminish "eldoc" 'eldoc-mode)
  (safe-diminish "flex-autopair" 'flex-autopair-mode)
  (safe-diminish "git-gutter" 'git-gutter-mode "GG")
  (safe-diminish "key-combo" 'key-combo-mode)
  (safe-diminish "paredit" 'paredit-mode "()")
  (safe-diminish "projectile" 'projectile-mode)
  (safe-diminish "volatile-highlights" 'volatile-highlights-mode))

(use-package smart-compile
  :init
  (defconst smart-compile-alist
  '(("\\.c\\'"          . "gcc -O2 %f -lm -o %n")
    ("\\.[Cc]+[Pp]*\\'" . "g++ -O2 %f -lm -o %n")
    ("\\.java\\'"       . "javac %f")
    ("\\.f90\\'"        . "gfortran %f -o %n")
    ("\\.[Ff]\\'"       . "gfortran %f -o %n")
    ("\\.tex\\'"        . (tex-file))
    ("\\.pl\\'"         . "perl -cw %f")
    (emacs-lisp-mode    . (emacs-lisp-byte-compile))
    ("\\.hs\\'"         . "ghc -o %n %f")
    ) "...")
  :config
  (global-set-key "\C-cc" 'smart-compile)
  (define-key menu-bar-tools-menu [compile] '("Compile..." . smart-compile)))

(use-package quickrun
  :config
  ;; 結果の出力バッファと元のバッファを行き来したい場合は
  ;; ':stick t'の設定をするとよい
  (push '("*quickrun*") popwin:special-display-config)

  ;; よく使うならキーを割り当てるとよいでしょう
  (global-set-key (kbd "<f5>") 'quickrun)
  (global-set-key (kbd "<f6>") 'quickrun-compile-only))

(use-package flycheck
  :config
  (custom-set-variables
   '(flycheck-emacs-lisp-load-path load-path))
  (add-hook 'after-init-hook #'global-flycheck-mode))

;;; Projectile
(use-package projectile
  :config
  (projectile-global-mode))

(use-package tabbar
  :config
  (tabbar-mode 1))

(use-package jazzradio)

(use-package nyan-mode
  :config
  (nyan-mode))

(use-package haskell-emacs)

(use-package drag-stuff
  :config
  (drag-stuff-mode))

(use-package hardcore-mode
;;; 00_init-package.el ends here

;;; 00_init-package.el --- package conf
;;; Commentary:
;;; Code:
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; (setq package-user-dir "~/.emacs.d/elap")

(package-initialize)

(require 'use-package)

;; (use-package pallet
;;   :config
;;   (pallet-mode +1))

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
   '(ido-save-directory-list-file (locate-user-emacs-file ".cache/ido.last"))
   '(ido-ignore-extensions t)))

(use-package multiple-cursors
  :functions rrm/switch-to-multiple-cursors
  :config
  (setq mc/list-file (locate-user-emacs-file ".cache/.mc-lists.el")))


(use-package popwin
  :config
  ;;dired
  ;; (push '(dired-mode :position top) popwin:special-display-config)
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
  ;; (push '("\\*magit.*" :stick t :regexp t :height 25) popwin:special-display-config)
  ;; (push '("COMMIT-EDITMSG" :height 15) popwin:special-display-config)
  (push '("*compilation*" :regexp t) popwin:special-display-config)
  (push '("*ert*" :regexp t) popwin:special-display-config)
  (push '("*Codic Result*" :height 15) popwin:special-display-config)
  (setq popwin:close-popup-window-timer-interval 0.7)
  )

(use-package volatile-highlights
  :config
  (volatile-highlights-mode t))

(use-package ats-mode
  :mode ("\\.dats\\'" . ats-mode)
  :commands (ats-mode))

(use-package peep-dired)

(use-package sudo-ext)

(use-package open-junk-file)

(use-package anzu
  :config
  (global-anzu-mode +1))

(use-package helm-swoop)

(use-package expand-region)

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

(use-package editorconfig
  :config
  (editorconfig-mode 1)
  (setq editorconfig-indentation-alist (nconc editorconfig-indentation-alist '((fish-mode fish-indent-offset))))
  )

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
  )

(use-package quickrun
  :config
  ;; 結果の出力バッファと元のバッファを行き来したい場合は
  ;; ':stick t'の設定をするとよい
  (push '("*quickrun*") popwin:special-display-config))

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

;; (use-package haskell-emacs)
;; M-x haskell-emacs-init

(use-package drag-stuff
  :config
  (drag-stuff-mode))

(use-package ein
  :config
  (use-package ein-notebook)
  (use-package ein-subpackages))

(use-package flycheck
  :config
  (global-flycheck-mode 1))

;; (use-package historyf
;;   :config
;;   (add-to-list 'historyf-minor-modes 'elisp-slime-nav))

(use-package rust-mode
  :config
  (setq rust-format-on-save t)
  ;; (use-package flycheck-rust
  ;;   :config
  ;;   (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
  ;; (setq rust-rustfmt-bin )
  )

(use-package helm-gtags
  :config
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode)

  ;; customize
  (custom-set-variables
   '(helm-gtags-path-style 'relative)
   '(helm-gtags-ignore-case t)
   '(helm-gtags-auto-update t)))

(use-package sh-script
  :config
  (defun sh-mode-conf ()
    (interactive)
    (setq sh-basic-offset 2))
  (add-hook 'sh-mode-hook #'sh-mode-conf))

(use-package no-littering
  :config
  ;; (setq no-littering-etc-directory
  ;;       (expand-file-name "config/" user-emacs-directory))
  (setq no-littering-var-directory
        (expand-file-name "data/" user-emacs-directory)))

(use-package gitter
  :config
  )

(use-package markdown-preview-mode
  :config
  )

(use-package company
  :config
  (global-company-mode)
  (setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
  )

(provide '00_init-package)

;;; 00_init-package.el ends here

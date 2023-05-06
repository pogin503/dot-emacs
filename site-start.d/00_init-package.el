;;; 00_init-package.el --- package conf
;;; Commentary:
;;; Code:
(require 'package)
(require '00_init-vars)

(eval-and-compile
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
  (add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/") t)
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf)))

;; (setq package-user-dir "~/.emacs.d/elap")


(require 'use-package)
(require 'leaf)

(leaf leaf-keywords
  :ensure t
  :init
  ;; ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
  (leaf hydra :ensure t)
  (leaf el-get :ensure t)
  (leaf blackout :ensure t)

  :config
  ;; initialize leaf-keywords.el
  (leaf-keywords-init))

(leaf el-get
  :config
  (add-to-list 'el-get-recipe-path (expand-file-name "el-get" my-dotemacs-dir))
  )

;; (leaf feather
;;   :el-get conao3/feather.el
;;   :config (feather-mode))

(leaf leaf
  :config
  ;; 選択したS式をleafブロックにしたものを別バッファに表示
  ;; leaf-convert-replace-pop
  ;; 選択したS式をleafブロックに置換
  ;; leaf-convert-replace-regionが使える
  (leaf leaf-convert :ensure t)
  ;; leafのツリーを左に表示
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left))))

(use-package ag
  :ensure t)

;; (use-package ido
;;   :config
;;   (ido-mode +1)
;;   (custom-set-variables
;;    '(ido-enable-flex-matching t)
;;    '(ido-use-filename-at-point 'guess)
;;    '(ido-everywhere t)
;;    '(ido-use-faces nil)
;;    '(ido-save-directory-list-file (locate-user-emacs-file ".cache/ido.last"))
;;    '(ido-ignore-extensions t)))

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

(use-package jazzradio)

;; (use-package haskell-emacs)
;; M-x haskell-emacs-init

(use-package drag-stuff
  :config
  (drag-stuff-mode))

;; (use-package ein
;;   :config
;;   (use-package ein-notebook)
;;   ;; (use-package ein-subpackages)
;;   )

(use-package flycheck
  :config
  (global-flycheck-mode 1))

;; (use-package historyf
;;   :config
;;   (add-to-list 'historyf-minor-modes 'elisp-slime-nav))

(use-package helm-gtags
  :hook (
         (c-mode . helm-gtags-mode)
         (c++-mode . helm-gtags-mode)
         (asm-mode . helm-gtags-mode)
         )
  :config
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

;;
(leaf no-littering
  :when (run-hook-with-args-until-failure 'use-package--no-littering--pre-init-hook)
  :custom `((no-littering-etc-directory . ,(expand-file-name "config/" user-emacs-directory))
            (no-littering-var-directory . ,(expand-file-name "data/" user-emacs-directory)))
  :require t
  :config
  (when (run-hook-with-args-until-failure 'use-package--no-littering--pre-config-hook)
    (setq auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/")
                                            t)))
    (run-hooks 'use-package--no-littering--post-config-hook)))

(use-package markdown-preview-mode
  :config
  )

(use-package company
  :config
  (global-company-mode)
  (setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
  ;; Trigger completion immediately.
  ;; (setq company-idle-delay 0)

  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (setq company-show-numbers t)
  )


(use-package lsp-mode
  :ensure t
  :init
  (yas-global-mode)
  (setq read-process-output-max (* 1024 1024))
  :hook (
         (rust-mode . lsp)
         (rustic-mode . lsp)
         )
  ;; :bind ("C-c h" . lsp-des)
  :custom (lsp-rust-server 'rust-analyzer)
  )

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))


  (use-package erc
    :defer t
    :config (progn
              (use-package erc-hl-nicks :config (add-hook 'erc-mode-hook #'erc-hl-nicks-mode))
              (erc-autojoin-mode t)
              (erc-scrolltobottom-enable)
              (erc-scrolltobottom-mode t)
              (setq erc-autojoin-channels-alist '((".*\\.freenode.net" "#emacs" "#clojure"))
                    erc-hide-list '("JOIN" "PART" "QUIT" "NICK" "MODE")
                    erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                              "324"  "329"  "332"  "333"  "353" "477")
                    erc-server "irc.freenode.net"
                    erc-port 6667
                    erc-nick "pogin"
                    erc-track-position-in-mode-line t
                    erc-input-line-position -2
                    erc-prompt-for-password nil
                    erc-header-line-face-method nil
                    erc-server-coding-system '(utf-8 . utf-8)
                    erc-prompt ">"
                    erc-accidental-paste-threshold-seconds 0.5
                    erc-join-buffer 'bury)))

(use-package company-tabnine
  :config
  (add-to-list 'company-backends #'company-tabnine))

(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :init
  (setq pipenv-projectile-after-switch-function
        #'pipenv-projectile-after-switch-extended))

(use-package exec-path-from-shell
  ;; :defun (exec-path-from-shell-initialize)
  :custom
  ((exec-path-from-shell-variables
    . '(
        "XDG_CONFIG_HOME"
        "XDG_CACHE_HOME"
        "XDG_DATA_HOME"
        "XDG_STATE_HOME"
        "SHELL"
        "CARGO_HOME"
        )))
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  )

(leaf tree-sitter :ensure t)
(leaf tree-sitter-langs :ensure t)

(el-get 'sync)

(provide '00_init-package)
;;; 00_init-package.el ends here

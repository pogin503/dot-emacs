;;; 01_init-keybind.el --- keybind conf
;;; Commentary:
;;; Code:

(require 'mylib)
(use-package bind-key)
(progn
  (bind-key "C-M-k" (lambda ()
                      (interactive)
                      (kill-buffer (current-buffer))))
  (bind-key [?¥] [?\\])

  (if (version<= emacs-version "24.4")
      (bind-keys :map read-expression-map
                 ("TAB" . lisp-complete-symbol))
    (bind-keys :map read-expression-map
               ("TAB" . completion-at-point)))
  (bind-key "C-x s" 'save-buffer)
  (bind-key "C-z" 'undo)
  (bind-key "s-<right>" 'right-word)
  (bind-key "s-<left>" 'left-word)
  (bind-key "s-<up>" 'backward-paragraph)
  (bind-key "s-<down>" 'forward-paragraph)
  (bind-key "s-<backspace>" 'backward-kill-word)
  (bind-key "s-w" 'kill-ring-save)
  (bind-key "C-m" 'newline-and-indent)
  (bind-key "C-<tab>" 'indent-for-tab-command)
  ;; Font size
  (bind-key "C-+" 'text-scale-increase)
  (bind-key "C--" 'text-scale-decrease)
  ;; window操作
  (bind-key "C-x <up>" 'windmove-up)
  (bind-key "C-x <down>" 'windmove-down)
  (bind-key "C-x <left>" 'windmove-left)
  (bind-key "C-x <right>" 'windmove-right)
  (bind-key "C-t" 'my-other-window-or-split)
  (bind-key "M-," #'(lambda () (interactive) (insert "、")))
  (bind-key "C-x RET u" 'ucs-normalize-NFC-buffer)
  (bind-key "M-O" 'my-edit-previous-line)
  (bind-key "S-<return>" 'my-edit-next-line)
  (bind-key "C-S-<return>" 'my-edit-previous-line)
  (bind-key "C-x C-o" 'other-window)
  (bind-key [?\¥] [?\\])
  (bind-key [?\C-¥] [?\C-\\])
  (bind-key [?\M-¥] [?\M-\\])
  (bind-key [?\C-\M-¥] [?\C-\M-\\])
  ;; (use-package emacs-lisp-mode
  ;;   :config
  ;;   ;; (use-package historyf
  ;;   ;;   :config
  ;;   ;;   (define-key emacs-lisp-mode-map (kbd "C-c C-<left>") 'historyf-back)
  ;;   ;;   (define-key emacs-lisp-mode-map (kbd "C-c C-<left>") 'historyf-forward)
  ;;   ;;   )
  ;;   ;; (bind-keys (:map emacs-lisp-mode-map
  ;;   ;;                  ("C-;" . my-describe-function)))
  ;;   )
  ;; (define-key emacs-lisp-mode-map (kbd "C-;") 'my-describe-function)

  (use-package treemacs
    :config
    (bind-keys :map treemacs-mode-map
               ([mouse-1] . treemacs-single-click-expand-action)
               ("+" . my-make-directory)))

  (use-package redo+
    :bind (("C-S-z" . redo)
           ("M-S-z" . redo)))

  (use-package helm
    :functions helm-mini helm-M-x
    :config
    (bind-keys ("M-]" . helm-mini)
               ("s-]" . helm-mini)
               ("M-x" . helm-M-x)))

  (use-package nxml-mode
    :functions lgfang-toggle-level
    :config
    (bind-keys :map nxml-mode-map
               ("M-'" . lgfang-toggle-level)
               ([mouse-3] . lgfang-toggle-level)
               ("<backtab>" . lgfang-toggle-level)))

  (use-package emmet-mode
    :config
    (bind-keys :map emmet-mode-keymap
               ("C-<return>" . nil)))

  (use-package company
    :config
    (defun my-ruby-mode-keybinds ()
      :config
      (bind-keys :map company-active-map
                 ("C-n" . company-select-next)
                 ("C-p" . company-select-previous)
                 ("C-s" . company-filter-candidates)
                 ([tab] 'company-complete-selection))) ;; TABで候補を設定
    )

  (use-package esh-mode
    :config
    (defun my-set-eshell-conf ()
      (bind-keys :map eshell-mode-map
                 ("\C-a" . eshell-bol)
                 ("<up>" . eshell-previous-matching-input-from-input)
                 ("<down>" . eshell-next-matching-input-from-input)
                 ;; ([(meta return)] . ns-toggle-fullscreen)
                 ;; ([(meta return)] . (select-toggle-fullscreen))
                 )
      )
    ;; (use-package auto-complete
    ;;   :config
    ;;   (bind-keys :map eshell-mode-map
    ;;              ("C-i" . auto-complete))
    ;;   )
    )
  (use-package yasnippet
    :config
    (bind-keys :map yas-minor-mode-map
               ;; 既存スニペットを挿入する
               ("C-x i i" . yas-insert-snippet)
               ;; 新規スニペットを作成するバッファを用意する
               ("C-x i n" . yas-new-snippet)
               ;; 既存スニペットを閲覧・編集する
               ("C-x i v" . yas-visit-snippet-file)
               ("C-SPC" . 'yas-expand)
               ("TAB" . nil)))

  (use-package helm-gtags
    :config
    (bind-keys :map helm-gtags-mode-map
               ("M-t" . 'helm-gtags-find-tag)
               ("M-r" . 'helm-gtags-find-rtag)
               ("M-s" . 'helm-gtags-find-symbol)
               ("M-g M-p" . 'helm-gtags-parse-file)
               ("C-c <" . 'helm-gtags-previous-history)
               ("C-c >" . 'helm-gtags-next-history)
               ("M-," . 'helm-gtags-pop-stack)))

  (use-package dired
    :config
    ;; (define-key dired-mode-map "\C-m" 'my-dired-advertised-find-file)
    (bind-keys :map dired-mode-map
               ([M-up] . my-dired-up-directory)
               ;; ("RET" . dired-find-alternate-file)
               ;; ([mouse-1] . dired-mouse-find-file)
               )

    ;; (define-key dired-mode-map "s" 'my-dired-various-sort-change-or-edit)

    (use-package peep-dired
      :config
      (bind-keys :map dired-mode-map
                 ("\C-xx" . peep-dired))
      (bind-keys :map peep-dired-mode-map
                 ("\C-xx" . peep-dired)))

    (use-package wdired
      :config
      (bind-keys :map dired-mode-map
                 ("e" . wdired-change-to-wdired-mode)
                 ([mouse-2] . dired-mouse-find-file)))

    (use-package dired-reuse
      :config
      ;; (define-key dired-mode-map (kbd "RET") 'dired-reuse-buffer)
      ;; (define-key dired-mode-map [mouse-1] 'dired-reuse-buffer-mouse)
      ;; (define-key dired-mode-map [mouse-1] 'dired-mouse-find-file)
      ;; (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file

      ;; (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory

      (bind-keys :map dired-mode-map
                 ;; ([mouse-1] . dired-reuse-buffer-mouse)
                 ;; ("RET" . dired-reuse-buffer)
                 ("^" . (function
                         (lambda nil (interactive) (dired-reuse-buffer "..")))))))

  (use-package company
    :config
    (bind-key "C-M-i" 'company-complete))

  (use-package google-translate
    :config
    (bind-key "\C-ct" 'google-translate-at-point)
    (bind-key "\C-cT" 'google-translate-query-translate))

  (use-package paredit
    :config
    (bind-keys :map paredit-mode-map
               ("C-j" . eval-print-last-sexp)))

  (use-package expand-region
    :bind ("C-=" . er/expand-region))

  (use-package quickrun
    :config
    (bind-key "<f5>" 'quickrun)
    (bind-key "<f6>" 'quickrun-compile-only))

  ;; (use-package howm
  ;;   :config
  ;;   (bind-key "C-c , ," 'howm-menu)
  ;;   (bind-keys :map howm-mode-map
  ;;               ([tab] . action-lock-goto-next-link)
  ;;               ([(meta tab)] . action-lock-goto-previous-link)
  ;;               ("\C-c\C-c" . my-save-and-kill-buffer))
  ;;   )

  (define-key menu-bar-tools-menu [compile] '("Compile..." . smart-compile))
  (use-package cc-mode
    :config
    (bind-keys :map c-mode-map
               ("C-~" . duplicate-thing)))

  (use-package helm-swoop
    :config
    ;; When doing isearch, hand the word over to helm-swoop
    (bind-keys :map isearch-mode-map
               ("M-i" . helm-swoop-from-isearch))
    ;; From helm-swoop to helm-multi-swoop-all
    (bind-keys :map helm-swoop-map
               ("M-i" helm-multi-swoop-all-from-helm-swoop))
    )

  (use-package multiple-cursors
    :bind (("C-c C-S-c" . mc/edit-lines)
		   ("C->" . mc/mark-next-like-this)
		   ("C-<" . mc/mark-previous-like-this)
		   ("C-c C-<"  . mc/mark-all-like-this)
		   ("C-*"  . mc/mark-all-like-this)))

  (use-package anzu
    :bind  ("M-%" . anzu-query-replace-regexp))

  (use-package haskell-mode
    :bind ("<f8>" . haskell-navigate-imports))

  (use-package org
    :bind (
           ;; org-agendaはアジェンダ(行動計画)を立てるための機能
           ("C-c a" . org-agenda)
           ;; org-modeでのメモをする機能
		   ("C-c c" . org-capture)
           ;; orgモードを使って現在のファイル行へのリンクを保存する
		   ("C-c l" . org-store-link)
           ;; orgバッファのみを行き来するiswitchbコマンド
		   ("C-c b" . org-iswitchb)))

  ;; (use-package auto-complete
  ;;   :config
  ;;   (bind-keys :map ac-mode-map
  ;;              ("M-i" . auto-complete))
  ;;   (bind-keys :map ac-completing-map
  ;;              ("\t" . ac-complete)
  ;;              ;; Enterで補完をしないようにする
  ;;              ("\r" nil)))

  ;; (use-package git-gutter
  ;;   :config
  ;;   (bind-key ("C-x C-g") 'git-gutter:toggle)      ;; Toggle git-gutter
  ;;   (bind-key ("C-x v =") 'git-gutter:popup-hunk)  ;; Popup diff window

  ;;   (bind-key ("C-x p") 'git-gutter:previous-hunk) ;; Jump to previous hunk
  ;;   (bind-key ("C-x n") 'git-gutter:next-hunk)     ;; Jump to next hunk

  ;;   (bind-key ("C-x v s") 'git-gutter:stage-hunk)  ;; Stage current hunk
  ;;   (bind-key ("C-x v r") 'git-gutter:revert-hunk) ;; Revert current hunk
  ;;   )

  ;; (global-set-key "\C-cw" 'sdic-describe-word)
  ;; (global-set-key "\C-cW" 'sdic-describe-word-at-point)
  ;; (define-key sdic-inline-map "\C-c\C-p" 'sdic-inline-pos-tip-show)
  ;; | C-c w | 英単語の意味を調べる |
  ;; | C-c W | カーソルの位置の英単語の意味を調べる |

  ;; (keyboard-translate ?\C-h ?\C-?)
  )


;;; 01_init-keybind.el ends here

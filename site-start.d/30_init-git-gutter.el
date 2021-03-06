;;; 30_init-git-gutter --- 30_init-git-gutter
;;; Commentary:
;; This program is free software
;; @see https://github.com/syohex/emacs-git-gutter
;;; Code:

(require 'git-gutter)
;; (global-git-gutter-mode t)

;; ignore all spaces
(custom-set-variables
 '(git-gutter:separator-sign "|")
 '(git-gutter:modified-sign "  ") ;; two space
 '(git-gutter:added-sign "++")    ;; multiple character is OK
 '(git-gutter:deleted-sign "--")
 '(git-gutter:diff-option "")
 ;; '(git-gutter:diff-option "-w")
 )

(set-face-background 'git-gutter:modified "purple") ;; background color
(set-face-foreground 'git-gutter:added "green")
(set-face-foreground 'git-gutter:deleted "red")
(set-face-foreground 'git-gutter:separator "yellow")

;; | C-x C-g | git-gutterのトグル |
;; | C-x v = | diffのウィンドウをポップアップさせて変更点を見る |
;; | C-x p   | 前のハンクに移動 |
;; | C-x n   | 次のハンクに移動 |
;; | C-x v s | 現在のハンクをインデックスに追加 |
;; | C-x v r | 現在のハンクの変更を取り消す |

(add-hook 'cperl-mode-hook 'git-gutter-mode)
(add-hook 'js2-mode-hook   'git-gutter-mode)
(add-hook 'php-mode-hook   'git-gutter-mode)
(add-hook 'ruby-mode-hook  'git-gutter-mode)
(add-hook 'emacs-lisp-mode-hook  'git-gutter-mode)

(provide '30_init-git-gutter)
;;; 30_init-git-gutter ends here

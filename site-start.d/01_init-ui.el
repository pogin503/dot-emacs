;;; 01_init-ui.el --- 01_init-ui.el -*- lexical-binding: t; coding: utf-8 -*-
;; Author: iMac
;; Version:
;; Package-Requires: ()
;;; Commentary:
;; This program is free software
;;; Code:

;; 背景の透過
(set-frame-parameter nil 'alpha 100)

;; タブ文字、全角空白、文末の空白の色付け
;; @see http://www.emacswiki.org/emacs/WhiteSpace
;; @see http://xahlee.org/emacs/whitespace-mode.html
(setq whitespace-style '(spaces tabs newline space-mark tab-mark newline-mark))

;; 現在行に色を付ける
(hl-line-mode 1)
(global-hl-line-mode)

(load-theme 'solarized-light t)

(use-package tabbar
  :config
  (tabbar-mode 1))
-
(use-package nyan-mode
  :config
  (nyan-mode))

;; フレームタイトルの設定
(setq frame-title-format (format "%%b - %s-%s" (invocation-name) emacs-version))

(custom-set-variables
 '(truncate-lines t)                 ; バッファ画面外文字の切り詰め表示
 '(truncate-partial-width-windows 0) ; ウィンドウ縦分割時のバッファ画面外文字の切り詰め表示
 )

(provide '01_init-ui)
;;; 01_init-ui.el ends here

(set-language-environment "Japanese")
(mw32-ime-initialize)
(setq default-input-method "MW32-IME")


;;@see http://sites.google.com/site/shidoinfo/Home/開発環境/emacs/emacsの基本
;======================================================================
; 言語・文字コード関連の設定
;======================================================================
(when (equal emacs-major-version 21) (req un-define))
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq file-name-coding-system 'utf-8)
;;
;======================================================================
; IMEの設定
;======================================================================
(mw32-ime-initialize)
(setq default-input-method "MW32-IME")
(setq-default mw32-ime-mode-line-state-indicator "[--]")
(setq mw32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
(add-hook 'mw32-ime-on-hook
	  (function (lambda () (set-cursor-height 2))))
(add-hook 'mw32-ime-off-hook
	  (function (lambda () (set-cursor-height 4))))
;;
;=======================================================================
;フォント
;=======================================================================
(if (eq window-system 'w32) (req ntemacs-font))
(fixed-width-set-fontset "msgothic" 14)

(create-fontset-from-ascii-font
 "-outline-メイリオ-normal-r-normal-normal-12-*-*-*-*-*-iso8859-1"
 nil "メイリオ")

(create-fontset-from-ascii-font
 "-outline-メイリオ-normal-r-normal-normal-14-*-*-*-*-*-iso8859-1"
 nil "メイリオ")

(set-fontset-font "fontset-メイリオ"
 'japanese-jisx0208
 '("メイリオ*" . "jisx0208-sjis"))
(set-fontset-font "fontset-メイリオ"
 'katakana-jisx0201
 '("メイリオ*" . "jisx0201-katakana"))
;;
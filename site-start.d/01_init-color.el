;;; 01_init-color.el --- color conf
;;; Commentary:
;;; Code:

;; 背景の透過
;; (add-to-list 'default-frame-alist '(alpha . (85 20)))
;; (add-to-list 'default-frame-alist '(alpha . (95 85)))
;; (add-to-list 'default-frame-alist '(alpha . (100 90)))

(set-frame-parameter nil 'alpha 100)

;; タブ文字、全角空白、文末の空白の色付け
;; @see http://www.emacswiki.org/emacs/WhiteSpace
;; @see http://xahlee.org/emacs/whitespace-mode.html
(setq whitespace-style '(spaces tabs newline space-mark tab-mark newline-mark))

;; 現在行に色を付ける
(hl-line-mode 1)
(global-hl-line-mode)

(load-theme 'solarized-dark t)

(provide '01_init-color)
;;; 01_init-color.el ends here

;;; 40_init-flyspell --- 40_init-flyspell
;; This program is free software
;;; Commentary:
;;; Code:
;;@see http://lcw-pon.blogspot.com/2009/12/emacs.html#more
;; Aspell
;;; ispellをより高機能なaspellに置き換える
(setq flyspell-issue-welcome-flag nil)
(setq-default ispell-program-name "aspell")
(setq ispell-grep-command "grep")
;;; 日本語ファイル中のスペルチェックを可能にする
(eval-after-load "ispell"
  '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

;; Flyspell
;;; FlySpellの逐次スペルチェックを使用するモードの指定
(defun my-flyspell-mode-enable ()
  (flyspell-mode 1))
(setq flyspell-issue-welcome-flag nil)

(mapc
 (lambda (hook)
   (add-hook hook 'my-flyspell-mode-enable))
 '(changelog-mode-hook
   text-mode-hook
   latex-mode-hook)
   ;;; Flyspellの逐次スペルチェックをコメントにのみ使用するモード
   ;;; (コメントかどうかの判断は各モードによる
 )

;(mapc
; (lambda (hook)
;   (add-hook hook 'flyspell-prog-mode))
; '(c-mode-common-hook
;   emacs-lisp-mode-hook))
(provide '40_init-flyspell)
;;; 40_init-flyspell ends here

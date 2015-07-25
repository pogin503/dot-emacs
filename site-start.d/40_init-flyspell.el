;;; 40_init-flyspell --- 40_init-flyspell
;; This program is free software
;;; Commentary:
;;; Code:

;;@see http://lcw-pon.blogspot.com/2009/12/emacs.html#more

;;; 日本語ファイル中のスペルチェックを可能にする
(eval-after-load "ispell"
  '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

(defun my-flyspell-mode-enable ()
  (flyspell-mode 1))

(mapc
 (lambda (hook)
   (add-hook hook 'my-flyspell-mode-enable))
 '(changelog-mode-hook
   text-mode-hook
   latex-mode-hook))

(provide '40_init-flyspell)
;;; 40_init-flyspell ends here

;;; 30_init-grep --- 30_init-grep
;; This program is free software
;;; Commentary:
;;; Code:
(setq grep-host-defaults-alist nil) ;; これはおまじないだと思ってください
(setq grep-template "lgrep <C> -n <R> <F> <N>")
(setq grep-find-template "find . <X> -type f <F> -print0 | xargs -0 -e lgrep <C> -n <R> <N>")

(defvar quote-argument-for-windows-p t "enables `shell-quote-argument' workaround for windows.")
(defadvice shell-quote-argument (around shell-quote-argument-for-win activate)
  "workaround for windows."
  (if quote-argument-for-windows-p
      (let ((argument (ad-get-arg 0)))
	(setq argument (replace-regexp-in-string "\\\\" "\\\\" argument nil t))
	(setq argument (replace-regexp-in-string "'" "'\\''" argument nil t))
	(setq ad-return-value (concat "'" argument "'")))
    ad-do-it))

;; ;; lgrep で Shift_JIS を使うように設定
;; (setq grep-host-defaults-alist nil) ;; これはおまじないだと思ってください
;; (setq grep-template "lgrep -Ks -Os <C> -n <R> <F> <N>")
;; (setq grep-find-template "find . <X> -type f <F> -print0 | xargs -0 -e lgrep -Ks -Os <C> -n <R> <N>")

;; lgrep -i -n \ほ\げ sample.txt /dev/null
;; sample.txt:1:ほげほげほげ
;; sample.txt:2:ほげふがほげ
;; ======================

;; そうしたら*grep*バッファを編集します．普通に編集です．query-replaceを使うのもいいですね．

;; ======================
;; lgrep -i -n \ほ\げ sample.txt /dev/null
;; sample.txt:1:FooFooFoo
;; sample.txt:2:FooふがFoo
;; ======================

;; 編集したら，"C-c C-e"と入力します．すると，編集した個所が全てのファイルに反映されます．開いていないファイルにもちゃんと反映してくれます．

;; この状態だとまだ保存をしてないので，あとは"C-x s"してから"!"で全部のバッファを保存してしまいましょう．

;;grep-edit
(req grep-edit)

(provide '30_init-grep)
;;; 30_init-grep ends here

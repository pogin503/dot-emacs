(add-to-list 'load-path "~/.emacs.d/plugins/sdic/")
(add-to-list 'load-path "~/.emacs.d/etc/dict/")

;;; sdic-mode 用の設定
(autoload 'sdic-describe-word
  "sdic" "英単語の意味を調べる" t nil)
(global-set-key "\C-cw" 'sdic-describe-word)
(autoload 'sdic-describe-word-at-point
  "sdic" "カーソルの位置の英単語の意味を調べる" t nil)
(global-set-key "\C-cW" 'sdic-describe-word-at-point)

(setq sdic-eiwa-dictionary-list
;      '((sdicf-client "/home/ogin/.emacs.d/etc/dict/gene.sdic"))
      '((sdicf-client "~/.emacs.d/etc/dict/gene.sdic"))
      ;; 和英検索で使用する辞書
      sdic-waei-dictionary-list
;      '((sdicf-client "/home/ogin/.emacs.d/etc/dict/jedict.sdic")))
      '((sdicf-client "~/.emacs.d/etc/dict/jedict.sdic")))

;;文字エンコード
;(setq sdic-default-coding-system 'utf-8)

;; 文字色
(setq sdic-face-color "pink")

;;./configure --with-emacs=emacs --with-dictdir=~/.emacs.d/etc/dict --with-dicttype=sdic --with-lispdir=~/.emacs.d/plugins/sdic
;;sudo make install 
;;sudo make install-info
;;make dict
;;sudo make install-dict

;;@see http://hgw09.exblog.jp/12670557/
(require 'sdic-inline)
(sdic-inline-mode t) ; sdic-inline モードの起動
(setq sdic-inline-word-at-point-strict t)

; 辞書ファイルの設定
(setq sdic-inline-eiwa-dictionary "~/.emacs.d/etc/dict/gene.sdic")
(setq sdic-inline-waei-dictionary "~/.emacs.d/etc/dict/jedict.sdic")

;sdic tooltip
(require 'sdic-inline-pos-tip)
(setq sdic-inline-display-func 'sdic-inline-pos-tip-show)

(setq transient-mark-mode t)

(defun sdic-inline-pos-tip-show-when-region-selected (entry)
  (cond
   ((and transient-mark-mode mark-active)
    (funcall 'sdic-inline-pos-tip-show entry))
   (t
; (funcall 'sdic-inline-display-minibuffer entry)
    )))

(setq sdic-inline-search-func 'sdic-inline-search-word-with-stem)
(setq sdic-inline-display-func 'sdic-inline-pos-tip-show-when-region-selected)
(define-key sdic-inline-map "\C-c\C-p" 'sdic-inline-pos-tip-show)

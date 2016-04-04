;;; 30_init-sdic --- 30_init-sdic
;; This program is free software
;;; Commentary:
;;; Code:

(require 'sdic)

(add-to-list 'load-path (locate-user-emacs-file "plugins/sdic/"))
(add-to-list 'load-path (locate-user-emacs-file "etc/dict/"))

;;; sdic-mode 用の設定
(autoload 'sdic-describe-word "sdic" "英単語の意味を調べる" t nil)
(global-set-key "\C-cw" 'sdic-describe-word)

(autoload 'sdic-describe-word-at-point
  "sdic" "カーソルの位置の英単語の意味を調べる" t nil)
(global-set-key "\C-cW" 'sdic-describe-word-at-point)

;; | C-c w | 英単語の意味を調べる |
;; | C-c W | カーソルの位置の英単語の意味を調べる |

(setq sdic-eiwa-dictionary-list
      '((sdicf-client (locate-user-emacs-file "etc/dict/gene.sdic"))))

;; 和英検索で使用する辞書
(setq sdic-waei-dictionary-list
      '((sdicf-client (locate-user-emacs-file "etc/dict/jedict.sdic"))))

;;文字エンコード
;(setq sdic-default-coding-system 'utf-8)

;; 文字色
(setq sdic-face-color "pink")

;;@see http://hgw09.exblog.jp/12670557/
;; ポイント下にある単語の意味を辞書で引き、意味を自動でミニバッファに表示
(req sdic-inline)
(sdic-inline-mode t) ; sdic-inline モードの起動
(setq sdic-inline-word-at-point-strict t) ; ポイント下に単語がある場合のみマッチする

;; 辞書ファイルの設定
(setq sdic-inline-eiwa-dictionary (locate-user-emacs-file "etc/dict/gene.sdic"))
(setq sdic-inline-waei-dictionary (locate-user-emacs-file "etc/dict/jedict.sdic"))

;; sdic-inline を有効とするメジャーモードを収めたリスト。
;; sdic-inline-enable-modes
;; sdic-inline を有効とするテキストプロパティを収めたリスト。
;; sdic-inline-enable-faces
;; 以下に設定された正規表現とファイル名がマッチする場合、sdic-inline を有効とする。
;; sdic-inline-enable-filename-regex
;; この変数に指定された関数の実行結果が t を返す場合、sdic-inline を有効とする。
;; sdic-inline-enable-func

;; sdic tooltip
(req sdic-inline-pos-tip)
(setq sdic-inline-display-func 'sdic-inline-pos-tip-show)

(defun sdic-inline-pos-tip-show-when-region-selected (entry)
  (cond
   ((and transient-mark-mode mark-active)
    (funcall 'sdic-inline-pos-tip-show entry))
   (t
    ;; (funcall 'sdic-inline-display-minibuffer entry)
    )))

(setq sdic-inline-search-func 'sdic-inline-search-word-with-stem)
(setq sdic-inline-display-func 'sdic-inline-pos-tip-show-when-region-selected)
(define-key sdic-inline-map "\C-c\C-p" 'sdic-inline-pos-tip-show)

(provide '30_init-sdic)
;;; 30_init-sdic ends here

;;; 50_init-c --- 50_init-c-mode
;; This program is free software
;;; Commentary:
;;; Code:

(defun labo-indent ()
  (setq c-basic-offset 4
        tab-width 4
        indent-tabs-mode t))

(defun set-c-mode-common-conf ()
  ;; (electric-mode + 自動インデント + 欲張り削除) ";"を押すと現在の行を
  ;; 再インデントして自動的に改行をするモードのなる設定。これは強力すぎて扱いづらい。
  ;; (c-toggle-auto-hungry-state 1)

  ;; (electric-mode) ";"や"{"などをを入力した場合現在の行を自動インデントを有功にする
  ;; (c-toggle-electric-state 1)

  ;; (欲張り削除 + electric-mode)バックスペースなどの削除するキーを押すと
  ;; スペースを一気に消す欲張り削除機能とelecetic-modeをを有功にする
  (c-toggle-hungry-state 1)

  ;; この関数は廃れた機能 (obsoleteされた)ものなので、emacsのバージョンが22.1以上なら使わないこと
  ;; (c-toggle-auto-state 1)　obsoleted

  ;; (自動インデント) 改行をしたら次の行を自動でインデントしてくれる
  ;; (c-toggle-auto-newline 1)

  ;; C-m を newline-and-indentに設定する
  ;; しかしこれをするとEnterのキーマップまで影響するので
  ;; 大人しくC-jを使ったnewline-and-indentを使うほうが
  ;; (define-key c-mode-base-map ""\C-m" 'newline-and-indent)
  (c-set-style "stroustrup")
  ;; (flyspell-prog-mode)                        ;;flyspell-prog-mode(自動ispell機能)
  (setq c-basic-offset 2)
  (c-set-offset 'arglist-cont 0)
  (c-set-offset 'arglist-close 0)
  ;; (labo-indent)
  ;; (setq company-minimum-prefix-length 4) ; デフォルトは4
  ;; (c-set-offset 'arglist-intro 2)
  ;; (c-set-offset 'arglist-cont '+)
  ;; (customize-variable
  ;;  '(c-electric-pound-behavior (quote (alignleft))))
  )

(add-hook 'c-mode-common-hook 'set-c-mode-common-conf)

;; c-indent-level                ブロック内のCの文の字下げを指定します．周囲のブロックの字下げとは開き中 かっこのある行の字下げをいいます．
;; c-continued-statement-offset  if文のthen節やwhile文の本体のように，文の中で始まる文に加える字下げの数 を指定します．
;; c-brace-offset                開き中かっこで始まる行に加える字下げの数を指定します．
;; c-brace-imaginary-offset      他のテキストの後ろにある開き中かっこが，この行の先頭からどれだけ右にある と考えるかを指定します．
;; c-argdecl-indent              Cの関数の引数宣言の字下げを指定します．
;; c-label-offset                ラベルやcase，defaultのある文に加える字下げの数を指定します．
(require 'use-package)
(use-package flycheck
  :config
  (add-to-list 'flycheck-enabled-checkers 'c/c++-gcc))

(provide '50_init)
;;; 50_init-c ends here

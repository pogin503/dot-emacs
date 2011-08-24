(add-hook 'c-mode-common-hook
          '(lambda ()
             ;; センテンスの終了である ';' を入力したら、自動改行+インデント
             (c-toggle-auto-hungry-state 1)
             ;; RET キーで自動改行+インデント
             (define-key c-mode-base-map "\C-m" 'newline-and-indent)
			 (flyspell-prog-mode)
			 (c-set-style "stroustrup")
			 (indent-tabs-mode nil)
			 ))


;; C++
; ヘッダファイル(.h)をc++モードで開く
(setq auto-mode-alist
      (append '(("\\.h$" . c++-mode))
              auto-mode-alist))

;;@see http://blog.livedoor.jp/tek_nishi/archives/3197109.html
;;(setq align-c++-modes (cons 'objc-mode align-c++-modes))


;; ;;@see http://ja.w3support.net/index.php?db=so&id=663588
;; (defun my-c-mode-common-hook () 
;;   ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode 
;;   (c-set-offset 'substatement-open 0) 
;;   ;; other customizations can go here 
;;   (setq c++-tab-always-indent t) 
;;   (setq c-basic-offset 4)                  ;; Default is 2 
;;   (setq c-indent-level 4)                  ;; Default is 2 
;;   (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60)) 
;;   (setq tab-width 4) 
;;   (setq indent-tabs-mode t)  ; use spaces only if nil 
;;   ) 
;; (add-hook 'c-mode-common-hook 'my-c-mode-common-hook) 

;; ;;; タブ幅を 5 に設定
;; ;; (setq-default tab-width 5)

;; ;;@see http://milky.way-nifty.com/nuzou/2005/02/cmodetab4.html
;; (defun my-c-mode-hook ()
;;  (c-set-style "linux")
;;  (setq tab-width 4)
;;  (setq c-basic-offset tab-width))
;;  (add-hook 'c-mode-hook 'my-c-mode-hook)

;; c-indent-level                ブロック内のCの文の字下げを指定します．周囲のブロックの字下げとは開き中 かっこのある行の字下げをいいます． 
;; c-continued-statement-offset  if文のthen節やwhile文の本体のように，文の中で始まる文に加える字下げの数 を指定します． 
;; c-brace-offset                開き中かっこで始まる行に加える字下げの数を指定します． 
;; c-brace-imaginary-offset      他のテキストの後ろにある開き中かっこが，この行の先頭からどれだけ右にある と考えるかを指定します． 
;; c-argdecl-indent              Cの関数の引数宣言の字下げを指定します．
;; c-label-offset                ラベルやcase，defaultのある文に加える字下げの数を指定します．

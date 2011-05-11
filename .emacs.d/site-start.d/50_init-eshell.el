;; Eshell

;;@see http://d.hatena.ne.jp/kitokitoki/20110222/p2
;; emacs 起動時に eshell バッファも一つ用意する
(add-hook 'after-init-hook
          (lambda()
            (eshell)
            ;(switch-to-buffer "*scratch*")
	    ))

(defun my-toggle-term ()
  "eshell と直前のバッファを行き来する。C-u 付きで呼ぶと 今いるバッファと同じディレクトリに cd して開く"
  (interactive)
  (let ((ignore-list '("*Help*" "*Minibuf-1*" "*Messages*"
                       "*terminal<1>*" "*terminal<2>*" "*terminal<3>*"))
        (dir default-directory))
    (labels
        ((_my-toggle-term (target)
           (if (null (member (buffer-name (second target)) ignore-list))
               (if (equal "*eshell*" (buffer-name (window-buffer)))
                   (switch-to-buffer (second target))
                 (switch-to-buffer "*eshell*")
                 (when current-prefix-arg
                   (cd dir)
                   (eshell-interactive-print (concat "cd " dir "\n"))
                   (eshell-emit-prompt)))
             (_my-toggle-term (cdr target)))))
      (_my-toggle-term (buffer-list)))))


(global-set-key (kbd "<C-M-return>") 'my-toggle-term)

;;@see http://d.hatena.ne.jp/a666666/20110222/1298345699

;; 補完時に大文字小文字を区別しない
(setq eshell-cmpl-ignore-case t)
;; 確認なしでヒストリ保存
(setq eshell-ask-to-save-history (quote always))
;; 補完時にサイクルする
;(setq eshell-cmpl-cycle-completions t)
(setq eshell-cmpl-cycle-completions nil)
;;補完候補がこの数値以下だとサイクルせずに候補表示
;(setq eshell-cmpl-cycle-cutoff-length 5)
;; 履歴で重複を無視する
(setq eshell-hist-ignoredups t)
;; prompt 文字列の変更
 (setq eshell-prompt-function
       (lambda ()
         (concat "[" (user-login-name) "@" (system-name) " " (eshell/pwd) "]"
                 (if (= (user-uid) 0) "\n# " "\n$ "))))

;; (setq eshell-prompt-function
;;       (lambda ()
;;         (concat "[kyanny@kyanny-laptop2 "
;;                 (eshell/pwd)
;;                 (if (= (user-uid) 0) "]\n# " "]\n$ ")
;;                 )))
;; 変更した prompt 文字列に合う形で prompt の初まりを指定 (C-a で"$ "の次にカーソルがくるようにする)
;; これの設定を上手くしとかないとタブ補完も効かなくなるっぽい
(setq eshell-prompt-regexp "^[^#$]*[$#] ")
;; キーバインドの変更
(add-hook 'eshell-mode-hook
          '(lambda ()
             (progn
               (define-key eshell-mode-map "\C-a" 'eshell-bol)
               ; yasnippet マイナーモードだと eshell-cmpl-cycle-completions がバグるのでオフる。
	       ;:C-u - M-x yas/minor-mode と等価。;               
	       ;(yas/minor-mode -1)      
               (define-key eshell-mode-map (kbd "<up>") 'eshell-previous-matching-input-from-input)
               (define-key eshell-mode-map (kbd "<down>") 'eshell-next-matching-input-from-input)
               ;(define-key eshell-mode-map [(meta return)] 'ns-toggle-fullscreen)
               ;(define-key eshell-mode-map [(meta return)] (select-toggle-fullscreen))

               )
             ))
(define-key global-map (kbd "C-z") 'eshell)
;; エスケープシーケンスを処理
;; @see http://d.hatena.ne.jp/hiboma/20061031/1162277851
(autoload 'ansi-color-for-comint-mode-on "ansi-color"
          "Set `ansi-color-for-comint-mode' to t." t)
(add-hook 'eshell-load-hook 'ansi-color-for-comint-mode-on)
;; http://www.emacswiki.org/emacs-ja/EshellColor
(require 'ansi-color)
(require 'eshell)
(defun eshell-handle-ansi-color ()
  (ansi-color-apply-on-region eshell-last-output-start
                              eshell-last-output-end))
(add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color)

;;@see http://valvallow.blogspot.com/2011/02/eshell.html
(require 'pcomplete)
(add-to-list 'ac-modes 'eshell-mode)
(ac-define-source pcomplete
  '((candidates . pcomplete-completions)))

(defun my-ac-eshell-mode ()
  (setq ac-sources
        '(ac-source-pcomplete
          ac-source-words-in-buffer
          ac-source-dictionary)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (my-ac-eshell-mode)
            (define-key eshell-mode-map (kbd "C-i") 'auto-complete)))

(custom-set-faces
    '(eshell-prompt-face ((t (:foreground "maroon2" :bold nil)))))
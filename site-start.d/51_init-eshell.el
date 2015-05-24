;;; 51_init-eshell --- 51_init-eshell
;;; Commentary:
;; @see http://d.hatena.ne.jp/a666666/20110222/1298345699
;; @see http://d.hatena.ne.jp/kitokitoki/20110222/p2
;; @see http://d.hatena.ne.jp/hiboma/20061031/1162277851
;; @see http://valvallow.blogspot.com/2011/02/eshell.html
;; @see http://d.hatena.ne.jp/tomoya/20090601/1243817036
;; @see http://www.emacswiki.org/emacs-ja/EshellColor
;;; Code:

(require 'eshell)
(require 'em-cmpl)
(require 'em-hist)
(require 'em-prompt)
(require 'pcomplete)

(setq eshell-cmpl-ignore-case t)           ;; 補完時に大文字小文字を区別しない
(setq eshell-cmpl-cycle-completions nil)   ;; 補完時にサイクルする
;;(setq eshell-cmpl-cycle-cutoff-length 5) ;;補完候補がこの数値以下だとサイクルせずに候補表示
(setq eshell-hist-ignoredups nil)          ;; 履歴で重複を無視する

(defun my-eshell-promp-function ()
  (concat "[" (user-login-name) "@" (system-name) " " (eshell/pwd) "]"
          (if (= (user-uid) 0) "\n# " "\n$ ")))

(setq eshell-prompt-function 'my-eshell-promp-function)  ;; prompt 文字列の変更

;; これの設定を上手くしとかないとタブ補完も効かなくなるらしい
(setq eshell-prompt-regexp "^[^#$]*[$#] ")

(defun my-set-eshell-conf ()
  (define-key eshell-mode-map "\C-a" 'eshell-bol)
  (define-key eshell-mode-map (kbd "<up>") 'eshell-previous-matching-input-from-input)
  (define-key eshell-mode-map (kbd "<down>") 'eshell-next-matching-input-from-input)
  ;;(define-key eshell-mode-map [(meta return)] 'ns-toggle-fullscreen)
  ;;(define-key eshell-mode-map [(meta return)] (select-toggle-fullscreen))
  (define-key eshell-mode-map (kbd "C-i") 'auto-complete)
  )

(add-hook 'eshell-mode-hook 'my-set-eshell-conf)

;; エスケープシーケンスを処理
(autoload 'ansi-color-for-comint-mode-on "ansi-color"
  "Set `ansi-color-for-comint-mode' to t." t)

(add-hook 'eshell-load-hook 'ansi-color-for-comint-mode-on)

(require 'ansi-color)

(defun eshell-handle-ansi-color ()
  (ansi-color-apply-on-region eshell-last-output-start
                              eshell-last-output-end))

(add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color)

(custom-set-faces
 '(eshell-prompt-face ((t (:foreground "maroon2" :bold nil)))))

;; emacs 起動時に eshell バッファも一つ用意する
(defun my-switch-eshell-buf-to-other-buf ()
  (eshell)
  (switch-to-buffer "*init log*"))

(add-hook 'after-init-hook 'my-switch-eshell-buf-to-other-buf)

(defun my-toggle-term ()
  "eshell と直前のバッファを行き来する。C-u 付きで呼ぶと 今いるバッファと同じディレクトリに cd して開く"
  (interactive)
  (let ((ignore-list '("*Help*"
                       " *Minibuf-0*"
                       " *Minibuf-1*"
                       "*Messages*"
                       "*terminal<1>*"
                       "*terminal<2>*"
                       "*terminal<3>*"
                       "*compilation*"
                       "*Anything Log*"
                       "*Completions*"
                       "*anything*"
                       "*anything coplete*"))
        (dir default-directory))
    (labels
        ((_my-toggle-term (target)
           (if (null (member (buffer-name (cl-second target)) ignore-list))
               (if (equal "*eshell*" (buffer-name (window-buffer)))
                   (switch-to-buffer (cl-second target))
                   (switch-to-buffer "*eshell*")
                   (when current-prefix-arg
                     (cd dir)
                     (eshell-interactive-print (concat "cd " dir "\n"))
                     (eshell-emit-prompt)))
               (_my-toggle-term (cdr target)))))
      (_my-toggle-term (buffer-list)))))

(global-set-key (kbd "<C-M-return>") 'my-toggle-term)

(provide '51_init-eshell)
;;; 51_init-eshell ends here

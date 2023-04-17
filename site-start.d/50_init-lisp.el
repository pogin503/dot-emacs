;;; 50_init-lisp.el --- lisp conf
;;; Commentary:
;;; Code:

(eval-when-compile
  (require '00_init-macro)
  (require '00_init-vars))

(require 'mylib)

;; .elと.elcの新しい方をロードする
(setq load-prefer-newer t)

(defun my-set-elisp-conf ()
  (interactive)
  (show-paren-mode t)
  (paredit-mode t))

(add-hook 'emacs-lisp-mode-hook 'esk-remove-elc-on-save)
(add-hook 'emacs-lisp-mode-hook 'my-set-elisp-conf)

(defun my-set-lisp-conf ()
  (interactive)
  (show-paren-mode t)
  (set (make-local-variable 'lisp-indent-function)
       'common-lisp-indent-function)
  (paredit-mode t))

(add-hook 'lisp-mode-hook 'my-set-lisp-conf)

;; eldoc setting
(require 'eldoc)
;; (require 'eldoc-extension)
(setq eldoc-idle-delay 0.21)
(setq eldoc-echo-area-use-multiline-p t)

(require 'dash)
(--each '(emacs-lisp-mode-hook
          lisp-interaction-mode-hook
          ielm-mode-hook)
  (add-hook it 'turn-on-eldoc-mode))

;; paredit
(use-package paredit
  :config
  (--each '(emacs-lisp-mode-hook
            lisp-interaction-mode-hook
            ielm-mode-hook
            lisp-mode-hook)
    (add-hook it 'enable-paredit-mode)))


;; | コマンド | 解説               |
;; |----------+--------------------|
;; | C-M-f    | 次のS式へ移動する   |
;; | C-M-b    | 前のS式へ移動する   |
;; | C-M-d    | リストの内側へ入る  |
;; | C-M-u    | リストの外側へ出る  |
;; | C-M-SPC  | S式をマークする    |
;; | C-M-k    | S式をカットする    |
;; | C-M-t    | S式を入れ替える    |

;; | コマンド | 解説                                       |
;; |----------+--------------------------------------------|
;; | M-(      | 括弧で囲む                                 |
;; | M-s      | 括弧を外す                                 |
;; | M-<up>   | カーソルの左側をkillして、括弧を一段階外す |
;; | M-<down> | カーソルの右側をkillして、括弧を一段階外す |
;; | M-r      | カーソル位置のシンボルを外に出す           |

;; | コマンド                        | 解説                     |
;; |---------------------------------+--------------------------|
;; | C-), C-<right>                  | 閉じ括弧を一つ右にずらす |
;; | C-}, C-<left>                   | 閉じ括弧を左にずらす     |
;; | C-(, C-M-<left>, ESC C-<left>   | 開き括弧を左にずらす     |
;; | C-{, C-M-<right>, ESC C-<right> | 開き括弧を右にずらす     |

;; | コマンド  | 解説                            |
;; |-----------+---------------------------------|
;; | M-)       |                                 |
;; | M-S       | S式や"を分割する                |
;; | M-J       | 分割されたS式などを結合する     |
;; | C-c C-M-l | S式を                           |
;; | M-q       | 一つのS式全体を再インデントする |

(use-package elisp-slime-nav
  :ensure elisp-slime-nav
  :config
  (progn
	(dolist (hook '(emacs-lisp-mode-hook
                    ielm-mode-hook
                    lisp-interaction-mode-hook))
	  (add-hook hook 'turn-on-elisp-slime-nav-mode))))

;;; 50_init-lisp.el ends here

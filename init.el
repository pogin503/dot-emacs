;;; init.le --- init file
;;; Commentary:
;;; Code:
; 常時デバッグ状態
;; (setq debug-on-error t)
;; (setq debug-on-error nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eshell-prompt-face ((t (:foreground "maroon2" :bold nil))) t))

;;install elisp
(setq load-path (cons "~/.emacs.d/elisp" load-path))


;;auto-install*******************************
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/elisp/")
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)             ; 互換性確保

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(when (not (boundp 'user-emacs-directory))
  (setq user-emacs-directory (expand-file-name "~/.emacs.d/"))
  )

;; 引数を load-path へ追加
;; normal-top-level-add-subdirs-to-load-path はディレクトリ中の中で
;; [A-Za-z] で開始する物だけ追加するので、追加したくない物は . や _ を先頭に付与しておけばロードしない
;; dolist は Emacs 21 から標準関数なので積極的に利用して良い
(defun add-to-load-path (&rest paths)
  "Add to load path recursively.
`PATHS' Directorys you want to read recursively."
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; Emacs Lisp のPathを通す
(add-to-load-path
 ;; 初期設定ファイル
 "site-start.d"
 "plugins"
 "elisp"
 "elpa"
 "etc"
 "share"
 "private"
 "el-get"
 )

(let ((path "~/.emacs.d/site-start.d/00_init-macro.elc"))
  (when (file-regular-p path)
    (delete-file path)))

;;init-loader
(when (equal emacs-major-version 24)
(require 'init-loader)
  (init-loader-load (concat user-emacs-directory "site-start.d/")))

;; 00 一般設定
;; 10 起動前実行系
;; 20 関数定義
;; 30 追加機能系
;; 40 マイナーモード
;; 50 メジャーモード
;; 60
;; 90 起動後実行系

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(debug-on-error nil)
 '(display-time-mode nil)
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-path-style (quote relative))
 '(safe-local-variable-values (quote ((flycheck-mode -1) (auto-revert-mode 1) (org-startup-truncated . t) (org-startup-truncated) (auto-fill-mode -1) (eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook" (add-hook (quote write-contents-functions) (lambda nil (delete-trailing-whitespace) nil)) (require (quote whitespace)) "Sometimes the mode needs to be toggled off and on." (whitespace-mode 0) (whitespace-mode 1)) (whitespace-style face trailing lines-tail) (require-final-newline . t) (ruby-compilation-executable . "ruby") (ruby-compilation-executable . "ruby1.8") (ruby-compilation-executable . "ruby1.9") (ruby-compilation-executable . "rbx") (ruby-compilation-executable . "jruby") (whitespace-line-column . 80) (lexical-binding . t))))
 '(session-use-package t nil (session))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(yas-trigger-key "TAB"))

;;@see http://d.hatena.ne.jp/zqwell-ss/20100620/1277025809

;;; init.el ends here

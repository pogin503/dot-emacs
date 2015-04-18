;;; init.le --- init file
;;; Commentary:
;;; Code:
;; デバッグモード
;; (setq debug-on-error t)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(when (not (boundp 'user-emacs-directory))
  (setq user-emacs-directory (expand-file-name "~/.emacs.d/")))

(setq load-path (cons (concat user-emacs-directory "elisp/") load-path))

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
 "etc"
 "share"
 "private"
 "el-get"
 )

(let ((path (concat user-emacs-directory "site-start.d/00_init-macro.elc")))
  (when (file-regular-p path)
    (delete-file path)))

;; init-loader
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



;; Custom path
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;;; init.el ends here

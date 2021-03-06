;;; init.le --- init file
;;; Commentary:
;;; Code:
;; デバッグモード
;; (setq debug-on-error t)

;; emacs -l init.el のように起動されるとload-file-nameにinit.elのパスが入るので
(when load-file-name
  ;; 設定ファイルの基準となるディレクトリを読み込んだinit.elのあるディレクトリへ変更する
  (setq user-emacs-directory (file-name-directory load-file-name)))

(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))

(setq load-path (cons (locate-user-emacs-file "elisp") load-path))

;; 引数を load-path へ追加
;; normal-top-level-add-subdirs-to-load-path はディレクトリ中の中で
;; [A-Za-z] で開始する物だけ追加するので、追加したくない物は . や _ を先頭に付与しておけばロードしない
;; dolist は Emacs 21 から標準関数なので積極的に利用して良い
(defun add-to-load-path (&rest paths)
  "Add to load path recursively.
`PATHS' Directorys you want to read recursively."
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (locate-user-emacs-file path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

(add-to-load-path
 ;; 初期設定ファイル
 "site-start.d"
 "plugins"
 "elisp"
 "etc"
 "el-get")

(let ((path (locate-user-emacs-file "site-start.d/00_init-macro.elc")))
  (when (file-regular-p path)
    (delete-file path)))

;; Custom path
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file)

;; init-loader
(when (equal emacs-major-version 24)
  (require 'init-loader)
  (init-loader-load (locate-user-emacs-file "site-start.d/"))
  (when noninteractive
    (init-loader-show-log)
    (with-current-buffer (get-buffer "*init log*")
      (print (buffer-string)))))

;; 00 一般設定
;; 10 起動前実行系
;; 20 関数定義
;; 30 追加機能系
;; 40 マイナーモード
;; 50 メジャーモード
;; 60
;; 90 起動後実行系

;;; init.el ends here

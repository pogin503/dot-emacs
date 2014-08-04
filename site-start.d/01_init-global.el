;;; 01_init-global --- global setting
;; This program is free software
;;; Commentary:
;;; Code:

(eval-when-compile
  (require '00_init-hanbetu))

;; ------------------------------------------------------------------------
;; @ frame

;; フレームタイトルの設定
(setq frame-title-format (format "%%b - %s-%s@%s" (invocation-name) emacs-version (system-name)))

;; ------------------------------------------------------------------------
;; @ buffer

;; バッファ画面外文字の切り詰め表示
(setq truncate-lines t)

;; ウィンドウ縦分割時のバッファ画面外文字の切り詰め表示
(setq truncate-partial-width-windows t)

;; 同一バッファ名にディレクトリ付与
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")


;; ------------------------------------------------------------------------
;; @ modeline

;;行番号の表示
(line-number-mode t)

;;; 列番号を表示
(column-number-mode t)

;; 時刻の表示
;; (require 'time)
;; (setq display-time-24hr-format t)
;; (setq display-time-string-forms '(24-hours ":" minutes))
;; (display-time-mode nil)

;; ------------------------------------------------------------------------
;; @ default setting

;; 起動メッセージの非表示
(setq inhibit-startup-message t)

;; スタートアップ時のエコー領域メッセージの非表示
(setq inhibit-startup-echo-area-message -1)

;; ------------------------------------------------------------------------


;;; 対応する括弧をブリンク ()
(setq blink-matching-paren t)
(setq blink-matching-delay 1000)


;;highlight parenthesis
(show-paren-mode t)

;; ------------------------------------------------------------------------
;; @fringe

;;display line-number in buffer
;; (global-linum-mode t)

;;line-number's format
;; (set-face-attribute 'linum nil :foreground "red" :height 0.8)
;; (setq linum-format "%4d")
;; (setq linum-delay t)
;; (defadvice linum-schedule (around my-linum-schedule () activate)
;;   (run-with-idle-timer 0.2 nil #'linum-update-current))

;; ------------------------------------------------------------------------
;; @ misc

;;@see http://aikotobaha.blogspot.com/2011/02/emacsdotemacs.html
;; yes/no を y/n へ簡略化
(fset 'yes-or-no-p 'y-or-n-p)

;; スクロール時のカーソル位置の維持
(setq scroll-preserve-screen-position t)

;; C-x C-f での意味の無いパス表示をグレーアウトする
(file-name-shadow-mode t)

;;@see http://sites.google.com/site/shidoinfo/Home/開発環境/emacs/emacsの基本
;;カーソルが行頭にある場合も行全体を削除
(setq kill-whole-line nil)

(require 'mylib)

(add-hook 'after-save-hook 'make-file-executable)

;;ガベージコレクションの頻度を下げる 初期設定は4000000
;;@see http://www.fan.gr.jp/~ring/Meadow/meadow.html
(setq gc-cons-threshold 40000000)

;;regionの選択中にBackspaceを押すと消せるようにする
;;@see http://www.fan.gr.jp/~ring/Meadow/meadow.html#ys:backward-delete-region
(defadvice backward-delete-char-untabify
  (around ys:backward-delete-region activate)
  (if (and transient-mark-mode mark-active)
      (delete-region (region-beginning) (region-end))
    ad-do-it))


;;config warning-suppress-types
;;@see http://d.hatena.ne.jp/fu7mu4/20101027/1288191419
(setq warning-suppress-types nil)

;; Emacs 設定ディレクトリを設定。Emacs 22以下用
;; Emacs 23.1 以上では user-emacs-directory 変数が用意されているのでそれを利用
;; (when load-file-name
;;   (setq user-emacs-directory (file-name-directory load-file-name)))
(add-to-list 'load-path user-emacs-directory)

(add-hook 'kill-emacs-query-functions 'my-byte-compile-func)

(cond (window-system
       (setq x-select-enable-clipboard t)))

;;スクロール量について
;; (setq scroll-conservatively 1)
;; (setq scroll-step 1)
;; (setq next-screen-context-lines 1)

;; ;; マウスホイールでスクロール
;; (defun scroll-down-with-lines ()
;;   "" (interactive) (scroll-down 5))
;; (defun scroll-up-with-lines ()
;;    "" (interactive) (scroll-up 5))
;; (global-set-key [mouse-4] 'scroll-down-with-lines)
;; (global-set-key [mouse-5] 'scroll-up-with-lines)


;;スクロールバーの場所
;;(set-scroll-bar-mode 'left) ;; 左側
(set-scroll-bar-mode nil) ;; なし
;;(set-scroll-bar-mode 'right) ;; 右側


;;recentf-mode
(setq recentf-auto-cleanup 'never)
(when (require 'recentf nil t)
  (require 'undo-tree)
  (setq recentf-max-saved-items 1000)
  (setq recentf-exclude '(".recentf"))
  (setq recentf-auto-cleanup 10)
  (setq recentf-auto-save-timer
        (run-with-idle-timer 30 t 'recentf-save-list))
  (recentf-mode 1))


;;@see http://e-arrows.sakura.ne.jp/2010/02/vim-to-emacs.html
;;cua-mode
(cua-mode t)
(setq cua-enable-cua-keys nil) ;; 変なキーバインド禁止


;;msb-mode
(msb-mode 1)

;; ------------------------------------------------------------------------
;; @backup file

(setq backup-by-copying t)

;; (my-define-backup-directory)

(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; ;; 編集中ファイルのバックアップ先
;; (setq auto-save-file-name-transforms
;;       `((".*" ,temporary-file-directory t)))
;; (setq backup-directory-alist
;;       `((".*" . ,temporary-file-directory)
;;         ("\#.*" . ,temporary-file-directory)))
;; (setq auto-save-file-name-transforms
;;       `((".*" ,(expand-file-name "~/.emacs.d/.backup") t)
;;         ("\#.*" ,(expand-file-name "~/.emacs.d/.backup") t)))

(setq version-control nil)        ; 複数のバックアップを残します。世代。
(setq kept-new-versions 5)   ; 新しいものをいくつ残すか
(setq kept-old-versions 5)   ; 古いものをいくつ残すか
(setq delete-old-versions t) ; 確認せずに古いものを消す。
(setq vc-make-backup-files t) ; バージョン管理下のファイルもバックアップを作る。

(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))

(require 'mylib)

;; 行末のwhitespaceを削除
(add-hook 'before-save-hook 'delete-trailing-whitespace-with-exclude-pattern)
;; ファイル末尾の改行を削除
(add-hook 'before-save-hook 'my-delete-trailing-blank-lines)

;; save-buffer 時、buffer 末尾に空行が常にあるように
(setq-default require-final-newline t)

;;; バッファの最後でnewlineで新規行を追加するのかどうか
(setq next-line-add-newlines t)
;; ------------------------------------------------------------------------
;; @ image-library
(if run-windows
    (let ((dll-list '((xpm "libxpm.dll")
                      (png "libpng14.dll")
                      (jpeg "libjpeg.dll")
                      (tiff "libtiff3.dll")
                      (gif "libungif4.dll")
                      (svg "librsvg-2-2.dll")
                      (gdk-pixbuf "libgdk_pixbuf-2.0-0.dll")
                      (glib "libglib-2.0-0.dll")
                      (gobject "libgobject-2.0-0.dll"))))
      (if (< emacs-major-version 24)
          (setq image-library-alist dll-list)
        (setq dynamic-library-alist dll-list)
        )))

;; @see http://trey-jackson.blogspot.jp/2009/08/emacs-tip-32-completion-ignore-case-and.html
;; (setq completion-ignore-case t)

;; 大文字小文字を無視した補完をするかどうか
(setq read-buffer-completion-ignore-case t)
;; ミニバッファでファイル名補完の時、大文字・小文字を無視するかどうか
(setq read-file-name-completion-ignore-case t)

;; tool-barを使うか使わないか
(tool-bar-mode -1)

;; menu-barを使うかどうか
(if run-darwin
    (menu-bar-mode 1)
  (menu-bar-mode -1))

;; オートフィルモードにはさせない
(auto-fill-mode -1)


;; automatically save buffers associated with files on buffer switch
;; and on windows switch
(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-up (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-down (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-left (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-right (before other-window-now activate)
  (when buffer-file-name (save-buffer)))

(defalias 'deactivate-advice 'ad-deactivate-all)

;; ミニバッファの履歴を保存する
(savehist-mode 1)

;; ミニバッファの履歴の保存数を増やす
(setq history-length 3000)

;; 行間
;; (setq-default line-spacing 0)

;; (add-hook 'prog-mode-hook 'esk-pretty-lambdas)
;; (add-hook 'prog-mode-hook 'esk-add-watchwords)

;; (set-default 'indicate-empty-lines nil)
(set-default 'imenu-auto-rescan t)

;; @indent setting
(setq-default c-basic-offset 4       ;;基本インデント量4
              tab-width 4            ;;タブ幅4
              indent-tabs-mode nil)  ;;インデントをタブでするかスペースでするか

(global-set-key (kbd "C-t") 'other-window-or-split)

;;highlight-cl
(require 'highlight-cl)
(add-hook 'emacs-lisp-mode-hook 'highlight-cl-add-font-lock-keywords)
(add-hook 'lisp-interaction-mode-hook 'highlight-cl-add-font-lock-keywords)


;;open-junk-file
(require 'open-junk-file)

;; windowを分割したときに折り返すときの値。
;; 非nilだと、変数の値の文字幅のときtruncate-lineしても
;; 折り返してしまう
(setq truncate-partial-width-windows nil)

;; path
(req exec-path-from-shell
    (exec-path-from-shell-initialize)
    )

;; rbenv path
;; (setenv "PATH" (concat (getenv "HOME") "/.rbenv/shims:"
;;                        (getenv "HOME") "/.rbenv/bin:" (getenv "PATH")))
;; (setq exec-path (cons (concat (getenv "HOME") "/.rbenv/shims")
;;                       (cons (concat (getenv "HOME") "/.rbenv/bin") exec-path)))

(provide '01_init-global)
;;; 01_init-global ends here

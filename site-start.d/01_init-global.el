;; ------------------------------------------------------------------------
;; @ frame

;; フレームタイトルの設定
(setq frame-title-format (format "%%b - %s-%s@%s" (invocation-name) "23.4" (system-name)))

;; ------------------------------------------------------------------------
;; @ buffer

;; バッファ画面外文字の切り詰め表示
(setq truncate-lines nil)

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
(require 'time)
(setq display-time-24hr-format t)
(setq display-time-string-forms '(24-hours ":" minutes))
(display-time-mode t)


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
(global-linum-mode t)

;;line-number's format
(set-face-attribute 'linum nil :foreground "red" :height 0.8)
(setq linum-format "%4d")

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
(setq kill-whole-line t)


;; スクリプトを保存する時、自動的に chmod +x を行うようにする
(defun make-file-executable ()
  "Make the file of this buffer executable, when it is a script source."
  (save-restriction
    (widen)
    (if (string= "#!"
                 (buffer-substring-no-properties 1
                                                 (min 3 (point-max))))
        (let ((name (buffer-file-name)))
          (or (equal ?. (string-to-char
                         (file-name-nondirectory name)))
              (let ((mode (file-modes name)))
                (set-file-modes name (logior mode (logand
                                                   (/ mode 4) 73)))
                (message (concat "Wrote " name " (+x)"))))))))
(add-hook 'after-save-hook 'make-file-executable)


;;ガベージコレクションの頻度を下げる 初期設定は4000000
;;@see http://www.fan.gr.jp/~ring/Meadow/meadow.html
(setq gc-cons-threshold 5242880)

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
(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory (expand-file-name "~/.emacs.d/")))

;;@see http://felyce.info/archives/blog/2010/12/emacs-25.html
;; 終了時バイトコンパイル

(defun my-byte-compile-func ()
  (if (file-newer-than-file-p (concat user-emacs-directory "init.el")
                              (concat user-emacs-directory "init.elc"))
      (byte-compile-file (concat user-emacs-directory "init.el")))
  (byte-recompile-directory (concat user-emacs-directory "elisp") 0)
  (byte-recompile-directory (concat user-emacs-directory "plugins") 0)
  (byte-recompile-directory (concat user-emacs-directory "site-start.d") 0)
  )
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
(recentf-mode 1)


;;@see http://e-arrows.sakura.ne.jp/2010/02/vim-to-emacs.html
;;cua-mode
(cua-mode t)
(setq cua-enable-cua-keys nil) ;; 変なキーバインド禁止


;;msb-mode
(msb-mode 1)

;; ------------------------------------------------------------------------
;; @backup file

(setq backup-by-copying t)
(defun my-define-backup-directory ()
  (let ((dir-name ".backup"))
    (if (not (file-exists-p (concat user-emacs-directory dir-name)))
        (make-directory dir-name user-emacs-directory))
    (add-to-list 'backup-directory-alist
                 `("" . ,(expand-file-name (concat user-emacs-directory dir-name))))))
(my-define-backup-directory)

(setq version-control t)        ; 複数のバックアップを残します。世代。
(setq kept-new-versions 5)   ; 新しいものをいくつ残すか
(setq kept-old-versions 5)   ; 古いものをいくつ残すか
(setq delete-old-versions t) ; 確認せずに古いものを消す。
(setq vc-make-backup-files t) ; バージョン管理下のファイルもバックアップを作る。

(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))

;; 行末のwhitespaceを削除
(setq delete-trailing-whitespace-exclude-patterns (list "\\.md$" "\\.markdown$" "\\.org$"))

(require 'cl)
(defun delete-trailing-whitespace-with-exclude-pattern ()
  (interactive)
  (cond ((equal nil (loop for pattern in delete-trailing-whitespace-exclude-patterns
                          thereis (string-match pattern buffer-file-name)))
         (delete-trailing-whitespace))))

(add-hook 'before-save-hook 'delete-trailing-whitespace-with-exclude-pattern)


;; ファイル末尾の改行を削除
;; http://www.emacswiki.org/emacs/DeletingWhitespace
(defun my-delete-trailing-blank-lines ()
  "Deletes all blank lines at the end of the file."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (delete-blank-lines))))

(add-hook 'before-save-hook 'my-delete-trailing-blank-lines)

;; save-buffer 時、buffer 末尾に空行が常にあるように
(setq require-final-newline t)

;; ------------------------------------------------------------------------
;; @ image-library
(if run-windows
    (setq image-library-alist
          '((xpm "libxpm.dll")
            (png "libpng14.dll")
            (jpeg "libjpeg.dll")
            (tiff "libtiff3.dll")
            (gif "libungif4.dll")
            (svg "librsvg-2-2.dll")
            (gdk-pixbuf "libgdk_pixbuf-2.0-0.dll")
            (glib "libglib-2.0-0.dll")
            (gobject "libgobject-2.0-0.dll"))
          ))

(defun toggle-truncate-lines ()
  "折り返し表示をトグル動作します."
  (interactive)
  (if truncate-lines
      (setq truncate-lines nil)
    (setq truncate-lines t))
  (recenter))

;; @see http://trey-jackson.blogspot.jp/2009/08/emacs-tip-32-completion-ignore-case-and.html
;; (setq completion-ignore-case t)

(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; tool-barを使うか使わないか
(tool-bar-mode -1)

;; menu-barを使うかどうか
(menu-bar-mode -1)

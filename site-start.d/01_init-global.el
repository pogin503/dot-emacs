;;; 01_init-global --- global setting
;; This program is free software
;;; Commentary:
;;; Code:

(eval-when-compile
  (require '00_init-hanbetu))

;; フレームタイトルの設定
(setq frame-title-format (format "%%b - %s-%s" (invocation-name) emacs-version))

(custom-set-variables
 '(truncate-lines t)                 ; バッファ画面外文字の切り詰め表示
 '(truncate-partial-width-windows 0) ; ウィンドウ縦分割時のバッファ画面外文字の切り詰め表示
 )

;; 同一バッファ名にディレクトリ付与
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")

;; 現在ポイントがある桁を`C-n'や`C-p'等で移動するときに桁の位置を固定して
;; 移動できるようにする。
;; C-x C-nで有効
;; C-u C-x C-nで無効化する
(put 'set-goal-column 'disabled nil)

(line-number-mode t)   ; 行番号の表示
(column-number-mode t) ; 列番号を表示

(setq inhibit-startup-message t)            ; 起動メッセージの非表示
(setq inhibit-startup-echo-area-message -1) ; スタートアップ時のエコー領域メッセージの非表示
;; (blink-cursor-mode 1)
;; (setq blink-matching-paren t)               ; 対応する括弧をブリンク
;; (setq blink-matching-delay 1000)
(show-paren-mode t)                         ; 括弧を強調表示する

;; (global-linum-mode t)

;;line-number's format
;; (set-face-attribute 'linum nil :foreground "red" :height 0.8)
;; (setq linum-format "%4d")
;; (setq linum-delay t)

;; ------------------------------------------------------------------------
;; @ misc

(fset 'yes-or-no-p 'y-or-n-p)            ; yes/no を y/n へ簡略化
(setq scroll-preserve-screen-position t) ; スクロール時のカーソル位置の維持
(file-name-shadow-mode t)                ; C-x C-f での意味の無いパス表示をグレーアウトする

(setq kill-whole-line nil) ; カーソルが行頭にある場合も行全体を削除

(require 'mylib)

(add-hook 'after-save-hook 'make-file-executable)

;;ガベージコレクションの頻度を下げる 初期設定は4000000
(setq gc-cons-threshold 40000000)

;; regionの選択中にBackspaceを押すと消せるようにする
;; @see http://www.fan.gr.jp/~ring/Meadow/meadow.html#ys:backward-delete-region
(defadvice backward-delete-char-untabify
  (around ys:backward-delete-region activate)
  (if (and transient-mark-mode mark-active)
      (delete-region (region-beginning) (region-end))
    ad-do-it))


;; config warning-suppress-types
;; @see http://d.hatena.ne.jp/fu7mu4/20101027/1288191419
(setq warning-suppress-types nil)

(add-hook 'kill-emacs-query-functions 'my-byte-compile-func)

(cond (window-system
       (setq x-select-enable-clipboard t)))

;; スクロールバーの場所
;; (set-scroll-bar-mode 'left) ; 左側
(set-scroll-bar-mode nil)   ; なし
;; (set-scroll-bar-mode 'right) ; 右側


;; @recentf-mode
(require 'recentf)
(recentf-mode 1)
(custom-set-variables
 ;; '(recentf-auto-cleanup 'never)
 '(recentf-max-saved-items 2000)
 '(recentf-auto-cleanup 600)
 '(recentf-exclude '(".recentf" "/elpa/" "/elisps/" "^/tmp/" "/\\.git/" "/\\.cask/"
                     "\\.mime-example" "\\.ido.last" "woman_cache.el"
                     "COMMIT_EDITMSG" "MERGE_MSG" "bookmarks" "\\.gz$"
					 "Command attempt to use minibuffer while in minibuffer")))


;; @see http://e-arrows.sakura.ne.jp/2010/02/vim-to-emacs.html
;; cua-mode
(cua-mode t)
(setq cua-enable-cua-keys nil)

(msb-mode 1)

;;バックアップファイルの作成場所を変更
(setq backup-directory-alist         `((".*" . ,temporary-file-directory)))

;;編集中ファイルのバックアップ先
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(custom-set-variables
 '(backup-by-copying       nil) ; いつもバックアップファイルを作るようにするかどうか
 '(version-control         nil) ; 複数のバックアップを残します。世代。
 '(kept-new-versions       5)   ; 新しいものをいくつ残すか
 '(kept-old-versions       5)   ; 古いものをいくつ残すか
 '(delete-old-versions     t)   ; 確認せずに古いものを消す
 '(vc-make-backup-files    nil) ; バージョン管理下のファイルもバックアップを作る
 '(make-backup-files       nil) ; バックアップファイルを作らないようにする
 '(delete-auto-save-files  t)   ; 終了時にオートセーブファイルを消す
 )

(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))

(add-hook 'before-save-hook 'delete-trailing-whitespace-with-exclude-pattern) ; 行末のwhitespaceを削除
(add-hook 'before-save-hook 'my-delete-trailing-blank-lines)                  ; ファイル末尾の改行を削除


(setq-default require-final-newline t) ; 保存時、バッファ末尾に空行が常にあるように
(setq next-line-add-newlines t)        ; バッファの最後でnewlineで新規行を追加するのかどうか
;; (setq completion-ignore-case t)        ; 非nilのとき大文字小文字を区別せずに補完する

(setq read-buffer-completion-ignore-case t)    ; 大文字小文字を無視した補完をするかどうか
(setq read-file-name-completion-ignore-case t) ; ミニバッファでファイル名補完の時、大文字・小文字を無視するかどうか

(tool-bar-mode -1)

;; menu-barを使うかどうか
(if run-darwin
    (menu-bar-mode 1)
  (menu-bar-mode -1))

(auto-fill-mode -1) ; 自動詰め込み(auto-file) モードにするかどうか

;; automatically save buffers associated with files on buffer switch
;; and on windows switch
(defun my-save-buffer ()
  (interactive)
  (when (and buffer-file-name
             (file-writable-p buffer-file-name))
    (save-buffer)))

(defadvice switch-to-buffer (before save-buffer-now activate)
  (my-save-buffer))
(defadvice other-window (before other-window-now activate)
  (my-save-buffer))
(defadvice windmove-up (before other-window-now activate)
  (my-save-buffer))
(defadvice windmove-down (before other-window-now activate)
  (my-save-buffer))
(defadvice windmove-left (before other-window-now activate)
  (my-save-buffer))
(defadvice windmove-right (before other-window-now activate)
  (my-save-buffer))

(defalias 'deactivate-advice 'ad-deactivate-all)

(savehist-mode 1)          ; ミニバッファの履歴を保存する
(setq history-length 3000) ; ミニバッファの履歴の保存数を増やす

;; 行間
;; (setq-default line-spacing 0)

(set-default 'imenu-auto-rescan t) ; いつも自動でリスキャンする

;; @indent setting
(setq-default c-basic-offset 4       ; 基本インデント量
              tab-width 4            ; タブ幅
              indent-tabs-mode nil)  ; インデントをタブでするかスペースでするか

(defvar dropbox-directory
  (cond
   ((eq run-windows t) (concat "c:/Users/" user-login-name "/Dropbox/"))
   (t "~/Dropbox/"))
  "Set Dropbox directory."
  )

;; path
(req exec-path-from-shell
    (exec-path-from-shell-initialize))

;; Cursor-type
;; Use a bar cursor when mark is active and a region exists.
(defun my-activate-mark-init ()
  "Use a bar cursor."
  (setq cursor-type 'bar))

(add-hook 'activate-mark-hook 'my-activate-mark-init)

(defun my-deactivate-mark-init ()
  "Use a box cursor."
  (setq cursor-type 'box))

(add-hook 'deactivate-mark-hook 'my-deactivate-mark-init)

(require 'server)
(unless (server-running-p)
  ;; (if (< emacs-major-version 23)
  (server-start))

;; M-x diff でunified diff形式で表示する。
(setq diff-switches "-u")

;; aliases
(defalias 'ms 'magit-status)
(defalias 'rr 'replace-regexp)

(provide '01_init-global)
;;; 01_init-global ends here

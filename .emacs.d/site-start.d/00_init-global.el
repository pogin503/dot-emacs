; 初期ディレクトリを設定
(setq default-directory "~/")


;;行番号の表示
(line-number-mode t)


;;; 列番号を表示
(column-number-mode t)


;; 常時デバッグ状態
(setq debug-on-error t)


;;; 対応する括弧をブリンク ()
(setq blink-matching-paren t)
(setq blink-matching-delay 1000)


;;highlight parenthesis
(show-paren-mode t)


;;@fringe

;;display line-number in buffer
(global-linum-mode t)


;;line-number's format
(set-face-attribute 'linum nil :foreground "red" :height 0.8)
(setq linum-format "%4d")


(require 'time)
(setq display-time-24hr-format t)
(setq display-time-string-forms '(24-hours ":" minutes))
(display-time-mode t)


;;起動時のmessageを表示しない
(setq inhibit-startup-message t)
;; scratch のメッセージを空にする
(setq initial-scratch-message nil)


;;@see http://aikotobaha.blogspot.com/2011/02/emacsdotemacs.html
; yes/no を y/n へ簡略化
(fset 'yes-or-no-p 'y-or-n-p)


; スクロール時のカーソル位置の維持
(setq scroll-preserve-screen-position t)


; C-x C-f での意味の無いパス表示をグレーアウトする
(file-name-shadow-mode t)


;;@see http://sites.google.com/site/shidoinfo/Home/開発環境/emacs/emacsの基本
;;カーソルが行頭にある場合も行全体を削除
(setq kill-whole-line t)                  


; スクリプトを保存する時、自動的に chmod +x を行うようにする
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

;; 安全な実行のための共通系関数

;; @see http://www.sodan.org/~knagano/emacs/dotemacs.html
(defmacro eval-safe (&rest body)
  "安全な評価。評価に失敗してもそこで止まらない。"
  `(condition-case err
       (progn ,@body)
     (error (message "[eval-safe] %s" err))))
(defun load-safe (loadlib)
  "安全な load。読み込みに失敗してもそこで止まらない。"
  ;; missing-ok で読んでみて、ダメならこっそり message でも出しておく
  (let ((load-status (load loadlib t)))
    (or load-status
        (message (format "[load-safe] failed %s" loadlib)))
    load-status))
(defun autoload-if-found (functions file &optional docstring interactive type)
  "set autoload iff. FILE has found."
  (if (not (listp functions))
      (setq functions (list functions)))
  (and (locate-library file)
       (progn
         (dolist (function functions)
           (autoload function file docstring interactive type))
         t )))


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


;;config warnig-suppress-types
;;@see http://d.hatena.ne.jp/fu7mu4/20101027/1288191419
(setq warning-suppress-types nil)

;; Emacs 設定ディレクトリを設定。Emacs 22以下用
;; Emacs 23.1 以上では user-emacs-directory 変数が用意されているのでそれを利用
(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory (expand-file-name "~/.emacs.d/")))

;;@see http://felyce.info/archives/blog/2010/12/emacs-25.html
;; 終了時バイトコンパイル
(add-hook 'kill-emacs-query-functions
          (lambda ()
	    (if (file-newer-than-file-p (concat user-emacs-directory "init.el")
					(concat user-emacs-directory "init.elc"))
                (byte-compile-file (concat user-emacs-directory "init.el")))
;;            (byte-recompile-directory (concat user-emacs-directory "elisp") 0)
;;            (byte-recompile-directory (concat user-emacs-directory "plugins") 0)
            (byte-recompile-directory (concat user-emacs-directory "site-start.d") 0)
            ))



(cond (window-system 
       (setq x-select-enable-clipboard t)))


;;スクロール量について
(setq scroll-conservatively 1)
(setq scroll-step 1)
(setq next-screen-context-lines 1)


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
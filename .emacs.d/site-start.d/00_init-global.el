;;行番号の表示
(line-number-mode t)


;;; 列番号を表示
(column-number-mode t)


;; 常時デバッグ状態
(setq debug-on-error t)


;;; 対応する括弧をブリンク ()
(setq blink-matching-paren t)
(setq blink-matching-delay 1)


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

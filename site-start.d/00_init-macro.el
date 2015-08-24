;;; 00_init-macro.el --- macro conf
;;; Commentary:
;;; Code:

;; ------------------------------------------------------------------------
;; @ macro

;;@see http://e-arrows.sakura.ne.jp/2010/03/macros-in-emacs-el.html
;;@see https://gist.github.com/304391
;;useful macros
(defmacro add-hook-fn (name &rest body)
  `(add-hook ,name #'(lambda () ,@body)))

;; 使用例
;; (add-hook-fn 'php-mode-hook
;;              (require 'symfony)
;;              (setq tab-width 2))

(defmacro append-to-list (to lst)
  `(setq ,to (append ,lst ,to)))

;; 使用例
;; (append-to-list exec-path
;;                 '("/usr/bin" "/bin"
;;                   "/usr/sbin" "/sbin" "/usr/local/bin"
;;                   "/usr/X11/bin"))

(defmacro req (lib &rest body)
  `(when (locate-library ,(symbol-name lib))
     (require ',lib) ,@body t))

;; 使用例
;; (req elscreen
;;   ...)

(defmacro lazyload (func lib &rest body)
  "Safe lazy load."
  `(when (locate-library ,lib)
     ,@(mapcar (lambda (f) `(autoload ',f ,lib nil t)) func)
     (eval-after-load ,lib
       '(progn
          ,@body)) t))

;; 使用例1
;; (lazyload (php-mode) "php-mode"
;;           (req symfony)
;;           (setq tab-width 2))

;;　使用例2
;; (lazyload (php-mode) "php-mode"
;;           (req symfony))
;; ;; add-hookは外に書く
;; (add-hook-fn 'php-mode-hook
;;              (setq tab-width 2)
;;              (c-set-offset 'arglist-intro '+)
;;              (c-set-offset 'arglist-close 0))

(defmacro global-set-key-fn (key &rest body)
  `(global-set-key ,key (lambda () (interactive) ,@body)))

;; 使用例
;; (global-set-key-fn (kbd "C-M-h") nil (interactive) (move-to-window-line 0))
;; (global-set-key-fn (kbd "C-M-m") nil (interactive) (move-to-window-line nil))
;; (global-set-key-fn (kbd "C-M-l") nil (interactive) (move-to-window-line -1))


;; 安全な実行のための共通系関数
;; @see http://www.sodan.org/~knagano/emacs/dotemacs.html
(defmacro eval-safe (&rest body)
  "安全な評価。評価に失敗してもそこで止まらない。"
  `(condition-case err
       (progn ,@body)
     (error (message "[eval-safe] %s" err))))

;; 使用例
;; (eval-safe (some-suspicious-code))
;; ;; nesting もできます。
;; (eval-safe
;;  (insert "1")
;;  (eval-safe
;;   (insert "2")
;;   (no-such-function))
;;  (insert "3")
;;  (no-such-function))

(defun load-safe (loadlib)
  "安全な load。読み込みに失敗してもそこで止まらない。"
  ;; missing-ok で読んでみて、ダメならこっそり message でも出しておく
  (let ((load-status (load loadlib t)))
    (or load-status
        (message (format "[load-safe] failed %s" loadlib)))
    load-status))

(defun autoload-if-found (functions file &optional docstring interactive type)
  "Set autoload iff. FILE has found."
  (if (not (listp functions))
      (setq functions (list functions)))
  (and (locate-library file)
       (progn
         (dolist (function functions)
           (autoload function file docstring interactive type))
         t )))
;; 使い方
;; 引数は autoload と全く同じです。-if-found を付けるだけ
;; (when (autoload-if-found 'bs-show "bs" "buffer selection" t)
;;   ;; autoload は成功した場合のみ non-nil を返すので、
;;   ;; when の条件部に置くことで、依存関係にある設定項目を自然に表現できます。
;;   (global-set-key [(control x) (control b)] 'bs-show)
;;   (setq bs-max-window-height 10))

(defmacro defun-add-hook (hookname &rest sexplist)
  "add-hook のエイリアス。引数を関数にパックして hook に追加する。"
  `(add-hook ,hookname
             (function (lambda () ,@sexplist))))
;; 使い方
;; 名前を defun で始めたのは、標準状態の Emacs-Lisp モードだと def? く
;; らいで始まるものは関数定義のスタイルでインデントされるからです。つ
;; まり、こんな感じ（何と2カラム目!）
;; (defun-add-hook 'perl-mode-hook
;;   (exec-if-bound (set-buffer-file-coding-system 'euc-japan-unix)))

(provide '00_init-macro)
;;; 00_init-macro.el ends here

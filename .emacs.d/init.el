;; 常時デバッグ状態
(setq debug-on-error t)

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
  "set autoload iff. FILE has found."
  (if (not (listp functions))
      (setq functions (list functions)))
  (and (locate-library file)
       (progn
         (dolist (function functions)
           (autoload function file docstring interactive type))
         t )))
;; 使い方
;; 引数は autoload と全く同じです。-if-found を付けるだけ
(when (autoload-if-found 'bs-show "bs" "buffer selection" t)
  ;; autoload は成功した場合のみ non-nil を返すので、
  ;; when の条件部に置くことで、依存関係にある設定項目を自然に表現できます。
  (global-set-key [(control x) (control b)] 'bs-show)
  (setq bs-max-window-height 10))


;;install elisp
(setq load-path (cons "~/.emacs.d/elisp" load-path))


;;auto-install*******************************
(req auto-install)
(setq auto-install-directory "~/.emacs.d/elisp/")
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)             ; 互換性確保


;;open-junk-file
(req open-junk-file)

;;debug config
;(add-hook 'after-init-hook
;          '(lambda () (setq debug-on-error t)))


;;reference from ALICE meadow
;;@see http://d.hatena.ne.jp/zqwell-ss/20100620/1277025809

;;; *scrach*をkill-bufferしたら自動復活させる
(defun my-make-scratch (&optional arg)
  (interactive)
  (progn
    ;; "*scratch*" を作成して buffer-list に放り込む
    (set-buffer (get-buffer-create "*scratch*"))
    (funcall initial-major-mode)
    (erase-buffer)
    (when (and initial-scratch-message (not inhibit-startup-message))
      (insert initial-scratch-message))
    (or arg (progn (setq arg 0)
                   (switch-to-buffer "*scratch*")))
    (cond ((= arg 0) (message "*scratch* is cleared up."))
          ((= arg 1) (message "another *scratch* is created")))))

(add-hook 'kill-buffer-query-functions
          ;; *scratch* バッファで kill-buffer したら内容を消去するだけにする
          (lambda ()
            (if (string= "*scratch*" (buffer-name))
                (progn (my-make-scratch 0) nil)
              t)))

(add-hook 'after-save-hook
          ;; *scratch* バッファの内容を保存したら *scratch* バッファを新しく作る
          (lambda ()
            (unless (member (get-buffer "*scratch*") (buffer-list))
              (my-make-scratch 1))))
;;; *scrach*をkill-bufferしたら自動復活させる ここまで

;;; ウィンドウのサイズを閉じる前に記憶しておく
(defun my-window-size-save ()
  (let* ((rlist (frame-parameters (selected-frame)))
         (ilist initial-frame-alist)
         (nCHeight (frame-height))
         (nCWidth (frame-width))
         (tMargin (if (integerp (cdr (assoc 'top rlist)))
                      (cdr (assoc 'top rlist)) 0))
         (lMargin (if (integerp (cdr (assoc 'left rlist)))
                      (cdr (assoc 'left rlist)) 0))
         buf
         (file "~/.framesize.el"))
    (if (get-file-buffer (expand-file-name file))
        (setq buf (get-file-buffer (expand-file-name file)))
      (setq buf (find-file-noselect file)))
    (set-buffer buf)
    (erase-buffer)
    (insert (concat
             ;; 初期値をいじるよりも modify-frame-parameters
             ;; で変えるだけの方がいい?
             "(delete 'width initial-frame-alist)\n"
             "(delete 'height initial-frame-alist)\n"
             "(delete 'top initial-frame-alist)\n"
             "(delete 'left initial-frame-alist)\n"
             "(setq initial-frame-alist (append (list\n"
             "'(width . " (int-to-string nCWidth) ")\n"
             "'(height . " (int-to-string nCHeight) ")\n"
             "'(top . " (int-to-string tMargin) ")\n"
             "'(left . " (int-to-string lMargin) "))\n"
             "initial-frame-alist))\n"
             ;;"(setq default-frame-alist initial-frame-alist)"
             ))
    (save-buffer)
    (kill-buffer ".framesize.el")
    ))

(defun my-window-size-load ()
  (let* ((file "~/.framesize.el"))
    (if (file-exists-p file)
        (load file))))

(my-window-size-load)

;;; Call the function above at C-x C-c.
(defadvice save-buffers-kill-emacs
  (before save-frame-size activate)
  (my-window-size-save))
;;; ウィンドウのサイズを閉じる前に記憶しておく ここまで

;;from ALICE meadow code end.


;;reference from sakito's config

;; 引数を load-path へ追加
;; normal-top-level-add-subdirs-to-load-path はディレクトリ中の中で
;; [A-Za-z] で開始する物だけ追加するので、追加したくない物は . や _ を先頭に付与しておけばロードしない
;; dolist は Emacs 21 から標準関数なので積極的に利用して良い
(defun add-to-load-path (&rest paths)
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
 "elisp/color-themes"
 "elisp"
 "plugins/slime")

;;from sakito's config end


;;行番号表示のelisp
;;(req wb-line-number)
;;(wb-line-number-toggle)


;;flymake
;(req flymake)
;(flymake-mode)


;;tabbar (tabberじゃなかった...)
(req tabbar)
(tabbar-mode)




;;highlight-cl
(req highlight-cl)
(add-hook 'emacs-lisp-mode-hook 'highlight-cl-add-font-lock-keywords)
(add-hook 'lisp-interaction-mode-hook 'highlight-cl-add-font-lock-keywords)



;;init-loader
(req init-loader)
(init-loader-load "~/.emacs.d/site-start.d/")

;; 00 一般設定
;; 10 起動前実行系
;; 20 関数定義
;; 30 追加機能系
;; 40 マイナーモード
;; 50 メジャーモード
;; 90 起動後実行系

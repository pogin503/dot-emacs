;;highlight parenthesis
(show-paren-mode t)

;;install elisp
(setq load-path (cons "~/.emacs.d/elisp" load-path))

;;auto-install*******************************
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/auto-install/")
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)             ; 互換性確保

;;auto-complete******************************
(require 'auto-complete)
(global-auto-complete-mode t)
(setq auto-install-directory "~/.emacs.d/elisp/")
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)

;;anything
(require 'anything)
(require 'anything-startup)
(require 'anything-config)
(require 'recentf)
;(add-to-list 'anything-sources 'anything-c-source-emacs-commands)
(define-key global-map (kbd "M-l") 'anything)
(setq recentf-max-saved-items 3000)
(recentf-mode t)
(setq anything-sources
      '(anything-c-source-buffers+
;        anything-c-source-colors
        anything-c-source-recentf
        anything-c-source-bookmarks
        anything-c-source-file-cache
        anything-c-source-man-pages
;        anything-c-source-emacs-variable-at-point
;        anything-c-source-emacs-function-at-point
        anything-c-source-file-name-history
;        anything-c-source-anything-grep-fallback
;        anything-c-source-anything-google-fallback
        anything-c-source-emacs-commands
        anything-c-source-emacs-functions
        anything-c-source-files-in-current-dir+
        ))

;;original key-bind
(define-key global-map (kbd "C-8")
  (lambda ()
    (interactive)
    (save-buffer)
    (load-file buffer-file-name)
    (message "load %S succeeded!" (current-buffer))))


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

;;; 列番号を表示
(column-number-mode t)

(require 'wb-line-number)
(wb-line-number-toggle)
;;; mylib.el --- mylib
;; This program is free software
;;; Commentary:
;;; Code:

(require '00_init-macro)

(eval-when-compile
  (require 'cl))

(defun print-escaped-sexp ()
  "Print escpaped s-expression."
  (interactive)
  (mark-defun)
  ;; (let ((beg (region-beginning)) (end (region-end)))
  ;;   (if (<= beg end)
  ;;       (copy-to-register ?r beg end)
  ;;     (copy-to-register ?r end beg)))
  (copy-to-register ?r (region-beginning) (region-end))
  (insert (format "%S" (substring-no-properties (get-register ?r))))
  )

(defun print-escaped-string (s e)
  "Print escaped string in region.

`S' is (region-beginning)
`E' is (region-end)"
  (interactive "r")
  (copy-to-register ?r s e)
  (insert (format "%S" (substring-no-properties (get-register ?r)))))

(defun reopen-buffer ()
  (interactive)
  (let ((current-buf (buffer-file-name)))
    (save-buffer)
    (kill-buffer)
    (find-file current-buf)
    ))

(if (not (fboundp 'file-name-base))
    (defun file-name-base (&optional filename)
      "Return the base name of the FILENAME: no directory, no extension.
FILENAME defaults to `buffer-file-name'."
  (file-name-sans-extension
   (file-name-nondirectory (or filename (buffer-file-name))))))

(defun my-insert-elisp-file-info ()
  "Insert Emacs Lisp header info."
  (interactive)
  (goto-char (point-min))
  (let ((f (file-name-nondirectory (buffer-file-name)))
        (f-noext (file-name-base)))
    (insert (format ";;; %s --- %s\n" f f))
    (insert (format ";; Author: %s\n" user-full-name))
    (insert ";; Version: \n")
    (insert ";; Package-Requires: ()\n")
    (insert ";;; Commentary:\n")
    (insert ";; This program is free software\n")
    (insert ";;; Code:\n\n")
    (goto-char (point-max))
    (save-excursion
      (insert (format "\n(provide '%s)\n" f-noext))
      (insert (format ";;; %s ends here\n" f))
    )))

(defun my-set-dev-env ()
  "For develop setting."
  (interactive)
  (custom-set-variables
   '(debug-on-error t)))

(defun my-unset-dev-env ()
  "Unset develop setting."
  (interactive)
  (custom-set-variables
   '(debug-on-error nil)))

;; @see https://gist.github.com/syohex/5487731
(defun parse-csv-file (file)
  (interactive
   (list (read-file-name "CSV file: ")))
  (let ((buf (find-file-noselect file))
        (result nil))
    (with-current-buffer buf
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (push (split-string line ",") result))
        (forward-line 1)))
    (reverse result)))

(defun parse-csv-string (str)
  (interactive)
  (let (;; (buf (find-file-noselect file))
        (result nil))
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (push (progn
                  (mapcar #'remove-dquote (split-string line ","))
                  ) result))
        (forward-line 1)))
    (reverse result)))

(defun remove-dquote (str)
  "Remove double quotes in `STR'."
  (cl-loop for i from 1 to (- (length str) 2)
                    concat (char-to-string (aref str i))
                    ))

(defun toggle-trancate-partial-width-windows ()
  (interactive)
  (if (null truncate-partial-width-windows)
      (setq truncate-partial-width-windows nil)
    (setq truncate-partial-width-windows t)))

(defun my-toggle-truncate-lines ()
  "折り返し表示をトグル動作します."
  (interactive)
  (if truncate-lines
      (setq truncate-lines nil)
    (setq truncate-lines t))
  (recenter))

(defconst my-truncate-t nil)
(defun my-toggle-truncate-setting ()
  (interactive)
  (if (eq my-truncate-t nil)
      (progn
        (setq truncate-partial-width-windows t)
        (setq truncate-lines t)
        (setq my-truncate-t t)
        (recenter)
        (message "Set truncate-lines t")
        )
    (progn
      (setq truncate-partial-width-windows nil)
      (setq truncate-lines nil)
      (setq my-truncate-t nil)
      (recenter)
      (message "Set truncate-lines nil")
      )
  ))

;; package
(defun my-install-package (pkgs)
  "Install my packages."
  (let ((not-installed (loop for x in pkgs
                             when (not (package-installed-p x))
                             collect x)))
    (when not-installed
      (package-refresh-contents)
      (dolist (pkg not-installed)
        (package-install pkg))))
  (message "done")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; haskell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(lazyload (haskell-mode) "hakskell-mode"
          (require 'auto-complete)
          (defun my-haskell-ac-init ()
            "Set AC-mode source."
            (when (member (file-name-extension buffer-file-name) '("hs" "lhs"))
              (auto-complete-mode t)
              (setq ac-sources '(ac-source-words-in-same-mode-buffers
                                 ac-source-dictionary
                                 ac-source-ghc-mod)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load "flycheck"
  '(defun my-toggle-flycheck-loadpath ()
    "Set load-paht to `flycheck-emacs-lisp-load-path'."
    (interactive)
    (if (null flycheck-emacs-lisp-load-path)
        (setq-default flycheck-emacs-lisp-load-path load-path)
      (setq-default flycheck-emacs-lisp-load-path nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar delete-trailing-whitespace-exclude-patterns
  (list "\\.md$" "\\.markdown$" "\\.org$"))

;; 行末のwhitespaceを削除
(defun delete-trailing-whitespace-with-exclude-pattern ()
  (interactive)
  (cond ((equal nil (loop for pattern in delete-trailing-whitespace-exclude-patterns
                          thereis (string-match pattern buffer-file-name)))
         (delete-trailing-whitespace))))

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

(defun my-define-backup-directory ()
  (let ((dir-name ".backup"))
    (if (not (file-exists-p (concat user-emacs-directory dir-name)))
        (make-directory dir-name user-emacs-directory))
    (add-to-list 'backup-directory-alist
                 `(".*" . ,(expand-file-name (concat user-emacs-directory dir-name))))))

(defun esk-pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                   nil))))))

;; TODO
(defun esk-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

(defun esk-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))

(defun my-load-current-buffer ()
  (interactive)
  (save-buffer)
  ;;  Tell me about all errors
  (if (boundp 'debug-ignored-errors)
      (setq debug-ignored-errors nil))
  (if (equal debug-on-error nil)
      (setq debug-on-error t))
  (load-file buffer-file-name)
  (message "load %S succeeded!" (current-buffer)))
(global-set-key (kbd "C-8") 'my-load-current-buffer)
(global-set-key (kbd "s-8") 'my-load-current-buffer)

;; ;; 'o' 次の行に挿入
;; (defun edit-next-line ()
;;   (interactive)
;;   (end-of-line)
;;   (newline-and-indent))

;; ;; 'O' 前の行に挿入
;; (defun edit-previous-line ()
;;   (interactive)
;;   (forward-line -1)
;;   (if (not (= (current-line) 1))
;;       (end-of-line))
;;   (newline-and-indent))

;; 'f' 後方の入力した文字の上に移動
(defun forward-match-char (n)
  (interactive "p")
  (let ((c (read-char)))
    (dotimes (i n)
      (forward-char)
      (skip-chars-forward (format "^%s" (char-to-string c))))))

;; 'F' 前方の入力した文字の上に移動
(defun backward-match-char (n)
  (interactive "p")
  (let ((c (read-char)))
    (dotimes (i n)
      (skip-chars-backward (format "^%s" (char-to-string c)))
      (backward-char))))

;;@see http://felyce.info/archives/blog/2010/12/emacs-25.html
;; 終了時バイトコンパイル
(defun my-byte-compile-func ()
  "Byte-compile files in particular directory."
  (interactive)
  (if (file-newer-than-file-p (concat user-emacs-directory "init.el")
                              (concat user-emacs-directory "init.elc"))
      (byte-compile-file (concat user-emacs-directory "init.el")))
  (byte-recompile-directory (concat user-emacs-directory "elisp") 0)
  (byte-recompile-directory (concat user-emacs-directory "plugins") 0)
  (byte-recompile-directory (concat user-emacs-directory "site-start.d") 0)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; coq
(defun my-ac-coq-mode ()
  "Set coq ac-source."
  (setq ac-sources '(ac-source-words-in-same-mode-buffers
                     ac-source-dictionary)))

;; anything or helm

;;anything-font-families
(lazyload (anything-execute-extended-command
           anything) "anything"
           (defun anything-font-families ()
             "Preconfigured `anything' for font family."
             (interactive)
             (flet ((anything-mp-highlight-match () nil))
               (anything-other-buffer
                '(anything-c-source-font-families)
                "*anything font families*")))

           (defun anything-font-families-create-buffer ()
             (with-current-buffer
                 (get-buffer-create "*Fonts*")
               (loop for family in (sort (delete-duplicates (font-family-list)) 'string<)
                     do (insert
                         (propertize (concat family "\n")
                                     'font-lock-face
                                     (list :family family :height 2.0 :weight 'bold))))
               (font-lock-mode 1)))

           (defvar anything-c-source-font-families
             '((name . "Fonts")
               (init lambda ()
                     (unless (anything-candidate-buffer)
                       (save-window-excursion
                         (anything-font-families-create-buffer))
                       (anything-candidate-buffer
                        (get-buffer "*Fonts*"))))
               (candidates-in-buffer)
               (get-line . buffer-substring)
               (action
                ("Copy Name" lambda
                 (candidate)
                 (kill-new candidate))
                ("Insert Name" lambda
                 (candidate)
                 (with-current-buffer anything-current-buffer
                   (insert candidate)))))))

(defun my-haskell-hook-function1 ()
  "Set haskell flex-autopair setting."
  (add-to-list 'flex-autopair-pairs '(?\' . ?\')))

(defun my-flex-sh-pair ()
  "Set shell flex-autopair setting."
  (add-to-list 'flex-autopair-pairs '(?\[ . ?\])))

;; ruby
(defun ruby-insert-end ()
  "Insert \"end\" at point and reindent current line."
  (interactive)
  (insert "end")
  (ruby-indent-line t)
  (end-of-line))


(defun my-ruby-resolve-warning ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp "\"" "'")
    (goto-char (point-min))
    (replace-regexp ":\\([a-zA-Z0-9]*\\)\s=>\s" "\\1: ")
    (goto-char (point-min))
    (replace-regexp "#\\([^ ]\\)" "# \\1")))

(defun my-md-replace ()
  (interactive)
  (let ((beg (point)))
    (save-excursion
      (replace-regexp "[[:blank:]]+" "|")
      (goto-char beg)
      (replace-regexp "^\\(.\\)" "|\\1")
      (goto-char beg)
      (replace-regexp "\\(.\\)$" "\\1|")
      )))

(defun nisshi ()
  (interactive)
  (let ((file (concat "~/workspace/日誌/"
                      (format-time-string "%Y-%m-%d.org" (current-time)))))
    (if (null (f-exists? file))
      (progn
        (f-touch file)
        (switch-to-buffer (find-file-noselect file))
        ))
    (switch-to-buffer (find-file-noselect file))))

(defun my-display-hashtable-data (hashtbl)
  (maphash #'(lambda (key val)
               (insert (format "%s, %s\n" key val))) hashtbl))

(defun my-ex-display-added-symbol ()
  (interactive)
  (mapconcat 'indentity
             (save-excursion
               (let (result)
                 (while (null (eobp))
                   (let ((q (re-search-forward "^(ex-put-example '" nil t))
                         (str (thing-at-point 'symbol)))
                     (if q
                         (push (with-temp-buffer
                                 (insert str)
                                 (buffer-substring-no-properties (point-min) (point-max)))
                               result)
                       (goto-char (point-max)))))

                 (reverse result))) "\n"))

(defun my-remove-comment ()
  (interactive)
  (replace-regexp " *;;? ?=> ?.+" "" nil (region-beginning) (region-end))
  (replace-regexp "^;;? ?.+" ""  nil (region-beginning) (region-end)))


(defun my-hash-exists-p (key table)
  (let ((novalue (make-symbol "<nil>")))
    (not (eq (gethash key table novalue) novalue))))

(defun my-insert-keybinds-table ()
  "init.el用のキーバインドの表を作る.

Example:
row> 3
;; |  |  |
;; |  |  |
;; |  |  |"
  (interactive)
  (let* ((row (string-to-number (read-from-minibuffer "row> "))))
    (cl-loop for i from 1 to row do
             (insert ";; ")
             (cl-loop for j from 1 to 2 do
                      (insert "|  ")
                      finally (insert "|\n"))
             )))

(provide 'mylib)
;;; mylib.el ends here

;;; mylib.el --- mylib
;; This program is free software
;;; Commentary:
;;; Code:

(require '00_init-macro)
(require '00_init-vars)

(require 'cl-lib)
(require 'package)
(require 'use-package)
(require 'f)

(defun my-print-escaped-sexp ()
  "Print escpaped s-expression."
  (interactive)
  (mark-defun)
  (copy-to-register ?r (region-beginning) (region-end))
  (insert (format "%S" (substring-no-properties (get-register ?r)))))

(defun my-print-escaped-string (s e)
  "Print escaped string in region."
  (interactive "r")
  (copy-to-register ?r s e)
  (kill-region s e)
  (insert (format "%S" (substring-no-properties (get-register ?r)))))

(defun my-reopen-buffer ()
  (interactive)
  (let ((current-buf (buffer-file-name)))
    (save-buffer)
    (kill-buffer)
    (find-file current-buf)))

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
    (insert (format ";;; %s --- %s -*- lexical-binding: t; coding: utf-8 -*-\n" f f))
    (insert (format ";; Author: %s\n" user-full-name))
    (insert ";; Version: \n")
    (insert ";; Package-Requires: ()\n")
    (insert ";;; Commentary:\n")
    (insert ";; This program is free software\n")
    (insert ";;; Code:\n\n")
    (goto-char (point-max))
    (save-excursion
      (insert (format "\n(provide '%s)\n" f-noext))
      (insert (format ";;; %s ends here\n" f)))))

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
                    concat (char-to-string (aref str i))))

(defun toggle-trancate-partial-width-windows ()
  (interactive)
  (if (null truncate-partial-width-windows)
      (setq truncate-partial-width-windows nil)
    (setq truncate-partial-width-windows t)))

(defconst my-truncate-t nil)
(defun my-toggle-truncate-setting ()
  (interactive)
  (if (eq my-truncate-t nil)
      (progn
        (setq truncate-partial-width-windows t)
        (setq truncate-lines t)
        (setq my-truncate-t t)
        (recenter)
        (message "Set truncate-lines t"))
    (setq truncate-partial-width-windows nil)
    (setq truncate-lines nil)
    (setq my-truncate-t nil)
    (recenter)
    (message "Set truncate-lines nil")))

;; package
(defun my-install-package (pkgs)
  "Install my packages."
  (let ((not-installed (cl-loop for x in pkgs
                             when (not (package-installed-p x))
                             collect x)))
    (when not-installed
      (package-refresh-contents)
      (dolist (pkg not-installed)
        (package-install pkg))))
  (message "done"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load "flycheck"
  '(defun my-toggle-flycheck-loadpath ()
    "Set load-paht to `flycheck-emacs-lisp-load-path'."
    (interactive)
    (if (null flycheck-emacs-lisp-load-path)
        (setq-default flycheck-emacs-lisp-load-path load-path)
      (setq-default flycheck-emacs-lisp-load-path nil))
	(revert-buffer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar delete-trailing-whitespace-exclude-patterns
  (list "\\.md$" "\\.markdown$" "\\.org$"))

;; 行末のwhitespaceを削除
(defun delete-trailing-whitespace-with-exclude-pattern ()
  (interactive)
  (cond ((equal nil (cl-loop for pattern in delete-trailing-whitespace-exclude-patterns
                          thereis (string-match pattern buffer-file-name)))
         (delete-trailing-whitespace))))

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

(defun my-define-backup-directory ()
  (let ((dir-name ".backup"))
    (if (not (file-exists-p (locate-user-emacs-file dir-name)))
        (make-directory dir-name user-emacs-directory))
    (add-to-list 'backup-directory-alist
                 `(".*" . ,(expand-file-name (locate-user-emacs-file dir-name))))))

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

(defun my-other-window-or-split ()
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

;; @see http://d.hatena.ne.jp/syohex/20120331/1333175819
;; 'o' 次の行に挿入
(defun my-edit-next-line ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

;; 'O' 前の行に挿入
(defun my-edit-previous-line ()
  (interactive)
  (forward-line -1)
  (if (not (= (current-line) 1))
      (end-of-line))
  (newline-and-indent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; anything or helm

;;anything-font-families
(lazyload (anything-execute-extended-command
           anything) "anything"
           (defun anything-font-families ()
             "Preconfigured `anything' for font family."
             (interactive)
             (cl-flet ((anything-mp-highlight-match () nil))
               (anything-other-buffer
                '(anything-c-source-font-families)
                "*anything font families*")))

           (defun anything-font-families-create-buffer ()
             (with-current-buffer
                 (get-buffer-create "*Fonts*")
               (cl-loop for family in (sort (cl-delete-duplicates (font-family-list)) 'string<)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-ruby-resolve-warning ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;; replace " -> '
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

(defun my-text-scale-increase ()
  (interactive)
  (let ((old-face-attribute (face-attribute 'default :height))
        new-size)
    (text-scale-increase 1)
    (setq new-size (/ (face-attribute 'default :height) 10))
    (message "+1 %d" new-size)))

(defun my-text-scale-decrease ()
  (interactive)
    (let ((old-face-attribute (face-attribute 'default :height))
          new-size)
      (text-scale-increase -1)
      (setq new-size (/ (face-attribute 'default :height) 10))
      (message "-1 %d" (- (/ old-face-attribute 10) 1))))

(defun my-display-hashtable-data (hashtbl)
  (maphash #'(lambda (key val)
               (message (format "%s, %s\n" key val))) hashtbl))

(defun my-hash-exists-p (key table)
  (let ((novalue (make-symbol "<nil>")))
    (not (eq (gethash key table novalue) novalue))))

(defun my-remove-comment ()
  (interactive)
  (replace-regexp " *;;? ?=> ?.+" "" nil (region-beginning) (region-end))
  (replace-regexp "^;;? ?.+" ""  nil (region-beginning) (region-end)))

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
                      finally (insert "|\n")))))

(defun my-insert-buffer-keybinds-table ()
  "バッファ中の設定されているキーバインドを表を作る.

global-set-key, define-key を探してそこからキーバインドの表を作る。

Example:
| C-a      | foo bar foo    |
| C-x C-x  | bar foo bar    |
| C-c C-c  | hoge piyo hoge |
...
"
  (interactive)
  (let (ret1 len)
    (save-excursion
      ;; [(keybind :: String . function-name :: String)]
      ;; バッファ中のキーバインド、関数名を取得する
      (setq ret1 (let (result break-flg)
                   (goto-char (point-min))
                   (while (and (not (eobp)) (not break-flg))
                     (if (re-search-forward "\\(^[[:space:]]*(global-set-key\\)\\|\\(^[[:space:]]*(define-key\\)" nil t)
                         (let (str1 str2 beg end end-next)
                           (beginning-of-defun-raw)
                           (setq beg (point))
                           (end-of-defun)
                           (narrow-to-region beg (point))
                           (goto-char (point-min))
                           (if (re-search-forward "kbd" nil t)
                               (progn
                                 (forward-char 2)
                                 (setq str1 (substring-no-properties (my-thing-at-string)))
                                 (if (re-search-forward "'" nil t)
                                     (setq str2 (substring-no-properties (thing-at-point 'symbol)))
                                   (setq str2 ""))))
                           (widen)
                           (push (list str1 str2) result)
                           (end-of-defun)
                           (setq end (point))
                           (end-of-defun)
                           (setq end-next (point))
                           (if (equal end end-next)
                               (setq break-flg t)
                             (goto-char end)))
                       (setq break-flg t)))
                   result)))
    ;; | keybind |  |
    ;;     .
    ;;     .
    ;;     .
    ;; を挿入する
    (insert (with-temp-buffer
              (save-excursion
                (mapc (lambda (x) (insert (format "|%s||\n" (car x)))) ret1))
              (org-table-align)
              (while (not (eobp))
                (beginning-of-line)
                (insert ";; ")
                (forward-line 1))
              (buffer-string)))))

;; @see http://d.hatena.ne.jp/IMAKADO/20091209/1260323922
(defun my-decamelize (string)
  "Convert from CamelCaseString to camel_case_string."
  (let ((case-fold-search nil))
    (downcase
     (replace-regexp-in-string
      "\\([A-Z]+\\)\\([A-Z][a-z]\\)" "\\1_\\2"
      (replace-regexp-in-string
       "\\([a-z\\d]\\)\\([A-Z]\\)" "\\1_\\2"
       string)))))

(defun my-camerize<->decamelize-on-region (s e)
  "リージョン中の文字列をキャメルケース⇔スネークケースで切り替える。"
  (interactive "r")
  (let ((buf-str (buffer-substring-no-properties s e))
        (case-fold-search nil))
    (cond
     ((string-match "_" buf-str)
      (let* ((los (mapcar 'capitalize (split-string buf-str "_" t)))
             (str (mapconcat 'identity los "")))
        ;; snake case to camel case
        (delete-region s e)
        (insert str)))
     (t
      (let* ((str (ik:decamelize buf-str)))
        ;; snake case to camel case
        (delete-region s e)
        (insert str))))))

(defun my-swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((/= (count-windows) 2)
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1))))
  (other-window 1))

(defun append-path (path)
  ""
  (setenv "PATH" (concat (file-truename path)":" (getenv "PATH")))
  (setq eshell-path-env (getenv "PATH"))
  (setq exec-path (split-string (getenv "PATH") ":"))
  (print exec-path))

;;; Notification center
(defun notif (title message)
  ""
  (shell-command
   (concat
    "echo 'display notification \"'"
    message
    "'\" with title \""
    title
    "\"' | osascript")))

(or (fboundp 'with-eval-after-load)
    (defmacro with-eval-after-load (feature &rest body)
      (declare (indent 1))
      `(eval-after-load ,feature
         '(progn ,@body))))

(defun describe-encoded-char (pos)
  "Describe how the character at POS is encoded by various coding-systems.
If a coding-system can't safely encode the character, display \"?\"."
  (interactive "d")
  (let ((coding-systems '(utf-8 utf-16be shift_jis cp932 iso-2022-jp))
        (label-fmt "%15s: ")
        (char (char-after pos))
        (byte-to-hex (lambda (byte) (format "%02x" byte))))
    (with-help-window (help-buffer)
      (with-current-buffer standard-output
        (insert (concat (format label-fmt "character") (char-to-string char) "\n"))
        (insert (concat (format label-fmt "code point")
                        (format (if (<= char #xffff) "U+%04x" "U+%06x") char) "\n"))
        (cl-loop for coding-system in coding-systems
                 do
                 (let* ((encoded-bytes (encode-coding-char char coding-system))
                        (encoded-hex (if encoded-bytes
                                         (concat "0x"
                                                 (mapconcat byte-to-hex encoded-bytes ""))
                                       "?")))
                   (insert (concat (format label-fmt coding-system) encoded-hex "\n"))))
        ))))

(require 's)
(defun buffer-lines ()
  "Get buffer lines."
  (s-split "\n" (buffer-substring-no-properties (point-min) (point-max))))

(defun words (str)
  "Split `STR' with a space."
  (s-split " " str))

(defun unwords (str)
  ""
  (s-join " " str))

(defun now ()
  "Insert string for the current time formatted like '22:34'."
  (interactive)
  (insert (format-time-string "%H:%M")))

(defun my-date-iso8601 ()
  "2014-10-10T13:50:40+09:00"
  (format-time-string "%Y-%m-%dT%H:%M:%S+09:00"))

(defun to-str (obj)
  ""
  (format "%s" obj))

(defun my-template ()
  ""
  (interactive)
  (let* ((date (format-time-string "%Y/%m/%d"))
         (header-char (cond ((string= major-mode "org-mode")
                             "*")
                            ((string= major-mode "markdown-mode")
                             "#")))
         (header-1 (s-repeat 1 header-char))
         (header-2 (s-repeat 2 header-char)))
    (insert (concat header-1 " " date
                    "\n"
                    header-2 " 前やったこと"
                    "\n"
                    header-2 " 今日やること"
                    "\n"
                    header-2 " 次回やること

============================
(1)()

"))))


(defun my-start-work ()
  ""
  (interactive)
  (find-file (concat dropbox-directory "/001_Documents/org/work.org"))
  (goto-char (point-max))
  (my-template)
  )

;; regionの選択中にBackspaceを押すと消せるようにする
;; @see http://www.fan.gr.jp/~ring/Meadow/meadow.html#ys:backward-delete-region
(defadvice backward-delete-char-untabify
    (around ys:backward-delete-region activate)
  ""
  (if (and transient-mark-mode mark-active)
      (delete-region (region-beginning) (region-end))
    ad-do-it))

(defun my-get-buffer-function ()
  ""
  (interactive)
  (let (ret (q t))
    (save-excursion
      (goto-char (point-min))
      (while q
        (setq q (re-search-forward "^\\((defun \\|(cl-defun \\)" nil t))
        (when q
          (push (substring-no-properties (thing-at-point 'symbol)) ret))))
    ret))

(defun current-line-number ()
  "Get the current line number (in the buffer) of point."
  (interactive)
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))

(use-package f
  :config
  ;;@see http://felyce.info/archives/blog/2010/12/emacs-25.html
  ;; 終了時バイトコンパイル
  (defun my-byte-compile-directory ()
    "Byte-compile files in particular directory."
    (interactive)
    (if (file-newer-than-file-p (locate-user-emacs-file "init.el")
                                (locate-user-emacs-file "init.elc"))
        (byte-compile-file (locate-user-emacs-file "init.el")))
    (byte-recompile-directory (locate-user-emacs-file "elisp") 0)
    (byte-recompile-directory (locate-user-emacs-file "plugins") 0)
    (byte-recompile-directory (locate-user-emacs-file "site-start.d") 0)
    (f-write (concat
              (with-current-buffer
                  (get-buffer "*Compile-Log*")
                (buffer-substring-no-properties (point-min) (point-max)))
              ";; Local variables:\n"
              ";; mode: compilation\n"
              ";; End:\n")
             'utf-8
             (concat "~/log/"  (format-time-string "emacs-compile-log_%Y-%m-%d %T.log"))
             )
    t)
  ;; (defun my-copy-current-line-info ()
  ;;   (interactive)
  ;;   (let ((path (f-this-file))
  ;;         ;; (root-path ((f-traverse-upwards
  ;;         ;;              (lambda (path)
  ;;         ;;                (f-exists? (f-expand ".git" path)))
  ;;         ;;              start-path)))
  ;;         )
  ;;     (when (not (null path))
  ;;       (kill-new (format "%s L%s\n"
  ;;                         (f-filename (f-this-file))
  ;;                         (current-line-number)
  ;;                         (buffer-substring-no-properties s e))))))

  (defun my-copy-current-line-info (rs re)
    (interactive "r")
    (let ((line-info (if (region-active-p)
                         (let ((line-start (save-excursion
                                             (goto-char rs)
                                             (current-line-number)))
                               (line-end (save-excursion
                                           (goto-char re)
                                           (if (bolp)
                                               (1- (current-line-number))
                                             (current-line-number)))))
                           (format "L%s-L%s" line-start line-end))
                       (format "L%s" (current-line-number))))
          (rs (if (region-active-p)
                 rs
               (save-excursion (beginning-of-line) (point))))
          (re (if (region-active-p)
                 re
               (save-excursion (end-of-line) (point)))))
      (let (result)
        (setq result (format "%s %s\n"
                           (f-filename (f-this-file))
                           line-info
                           ))
        (kill-new result)
        result)))
  (defun my-copy-current-line-info-with-code (rs re)
    (interactive "r")
    (let ((line-info (if (region-active-p)
                         (let ((line-start (save-excursion
                                             (goto-char rs)
                                             (current-line-number)))
                               (line-end (save-excursion
                                           (goto-char re)
                                           (if (bolp)
                                               (1- (current-line-number))
                                             (current-line-number)))))
                           (format "L%s-L%s" line-start line-end))
                       (format "L%s" (current-line-number))))
          (rs (if (region-active-p)
                 rs
               (save-excursion (beginning-of-line) (point))))
          (re (if (region-active-p)
                 re
               (save-excursion (end-of-line) (point)))))
      (let (result)
        (setq result (format "%s %s\n%s\n"
                           (f-filename (f-this-file))
                           line-info
                           (buffer-substring-no-properties rs re)))
        (kill-new result)
        result))))

;; @see http://stackoverflow.com/questions/1511737/how-do-you-list-the-active-minor-modes-in-emacs
(defun which-active-modes ()
  "Give a message of which minor modes are enabled in the current buffer."
  (interactive)
  (let ((active-modes))
    (mapc (lambda (mode) (condition-case nil
                             (if (and (symbolp mode) (symbol-value mode))
                                 (add-to-list 'active-modes mode))
                           (error nil) ))
          minor-mode-list)
    (message "Active modes are %s" active-modes)))

(defun no-properties-string (str)
  ""
  (with-temp-buffer
    (insert str)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun contains (elt list)
  ""
  (if (numberp (cl-position elt list))
      t
    nil))

;; @see https://github.com/howardabrams/dot-files/blob/master/elisp/shell-script-funcs.el
(defun substring-replace (old-str new-str beg end)
  "Return a new string where a subsection of OLD-STR has been replaced with NEW-STR beginning at position BEG and ending at END."
  (concat (substring old-str 0 beg) new-str (substring old-str end)))

;; @see https://github.com/rejeep/f.el#using-standard-emacs-builtin-functions
(defun find-git-root (&optional dir)
  (unless dir (setq dir (expand-file-name (file-name-directory (buffer-file-name)))))
  (let ((parent (expand-file-name ".." dir)))
    (unless (equal parent dir)
      (if (file-exists-p (expand-file-name ".git" dir))
          dir
        (find-git-root parent)))))

(defun my-qa-insert (q)
  "org-modeでQA形式のテンプレートを挿入する"
  (interactive "sQ: ")
  (move-beginning-of-line 1)
  (insert (format "** Q. [[#%s][%s]]\n" q q))
  (goto-char (point-max))
  (insert (format "** A. %s
   :PROPERTIES:
   :CUSTOM_ID: %s
   :END:
\n\n" q q))
  ;; :PROPERTIES: の行に移動する
  (forward-line -5)
  ;; :PROPERTIES: を隠す
  (org-cycle))

(defun revert-buffer-without-confirm ()
  (interactive)
  (revert-buffer nil t)
  (message "Buffer reverted."))

;; https://stackoverflow.com/questions/3034237/check-if-current-emacs-buffer-contains-a-string
(defun buffer-contains-substring (string)
  ""
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (search-forward string nil t))))

(defun my-sandbox-dir-dired ()
  ""
  (interactive)
  (let ((buf (dired (concat dropbox-directory "100_repo/ghworkspace"))))
    (switch-to-buffer buf))
  )

;;----------------------------------------------------------------------------
;; String utilities missing from core emacs
;;----------------------------------------------------------------------------
(defun sanityinc/string-all-matches (regex str &optional group)
  "Find all matches for `REGEX' within `STR', returning the full match string or group `GROUP'."
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))

(defun sanityinc/string-rtrim (str)
  "Remove trailing whitespace from `STR'."
  (replace-regexp-in-string "[ \t\n]+$" "" str))

  ;;;###autoload
(defun duplicate-thing (n)
  (interactive "P")
  (save-excursion
    (let (start
          end
          (with-comment-out (consp n)))
      (cond (mark-active
             (setq start (region-beginning) end (region-end)))
            (t
             (beginning-of-line)
             (setq start (point))
             (forward-line)
             (setq end (point))))
      (kill-ring-save start end)
      (if with-comment-out
          (progn
            (comment-region start end)
            (yank))
        (dotimes (i (or n 1))
          (yank))))))

;; @see https://gist.github.com/sky-y/3264252
(defun ucs-normalize-NFC-buffer ()
  "バッファ全体の濁点分離を直します."
  (interactive)
  (ucs-normalize-NFC-region (point-min) (point-max)))

(defun my-get-buffer-dir-path ()
  "Get buffer directory."
  (interactive)
  (if (f-this-file)
      (f-parent (f-this-file))
    (dired-current-directory)))

(provide 'mylib)
;;; mylib.el ends here

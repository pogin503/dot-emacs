;;; mylib --- mylib
;; This program is free software
;;; Commentary:
;;; Code:
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

(defun insert-elisp-file-info ()
  "Insert Emacs Lisp header info."
  (interactive)
  (goto-char (point-min))
  (let ((f (file-name-nondirectory (buffer-file-name)))
        (f-noext (file-name-base)))
    (insert (format ";;; %s --- %s\n" f f))
    (insert ";; This program is free software\n")
    (insert ";;; Commentary:\n")
    (insert ";;; Code:\n")
    (goto-char (point-max))
    (insert (format "(provide '%s)\n;;; %s ends here\n" f-noext f))
  ))

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
  (loop for i from 1 to (- (length str) 2)
                    ;; collect (char-to-string (aref str i))
                    concat (char-to-string (aref str i))
                    ))

(defun toggle-trancate-partial-width-windows ()
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
(require 'auto-complete)

(defun my-haskell-ac-init ()
  "Set AC-mode source."
  (when (member (file-name-extension buffer-file-name) '("hs" "lhs"))
    (auto-complete-mode t)
    (setq ac-sources '(ac-source-words-in-same-mode-buffers
                       ac-source-dictionary
                       ac-source-ghc-mod))))

;; (defun flymake-show-and-sit ()
;;   "Displays the error/warning for the current line in the minibuffer"
;;   (interactive)
;;   (progn
;;     (let* ( (line-no             (flymake-current-line-no) )
;; 	    (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
;; 	    (count               (length line-err-info-list))
;; 	    )
;;       (while (> count 0)
;; 	(when line-err-info-list
;; 	  (let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
;; 		 (full-file  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
;; 		 (text (flymake-ler-text (nth (1- count) line-err-info-list)))
;; 		 (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
;; 	    (message "[%s] %s" line text)
;; 	    )
;; 	  )
;; 	(setq count (1- count)))))
;;   (sit-for 60.0)
;;   )
;; ;;

;; (defun define-haskell-mode-conf ()
;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.l?hs$" flymake-Haskell-init flymake-simple-java-cleanup))
;;   (add-to-list 'flymake-err-line-patterns
;;                '("^\\(.+\\.l?hs\\):\\([0-9]+\\):\\([0-9]+\\):\\(\\(?:.\\|\\W\\)+\\)"
;;                  1 2 3 4))
;;   (set (make-local-variable 'multiline-flymake-mode) t)
;;   (if (not (null buffer-file-name)) (flymake-mode))
;;   )

;; (defun helm-c-source-ghc-mod ()
;;   (unless (executable-find "ghc-mod")
;;     (error "ghc-mod を利用できません。ターミナルで which したり、*scratch* で exec-path を確認したりしましょう"))
;;   (let ((buffer (helm-candidate-buffer 'global)))
;;     (with-current-buffer buffer
;;       (call-process "ghc-mod" nil t t "list"))))

;; (defun helm-c-source-ghc-mod-action (candidate)
;;   (interactive "P")
;;   (let* ((pkg (ghc-resolve-package-name candidate)))
;;     (helm-aif (and pkg candidate)
;;         (ghc-display-document pkg it nil)
;;       (message "No document found"))))

;; (defun helm-ghc-browse-document ()
;;   (interactive)
;;   (helm helm-c-source-ghc-mod))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-set-flycheck-loadpath ()
  "Set load-paht to `flycheck-emacs-lisp-load-path'."
  (interactive)
  (setq-default flycheck-emacs-lisp-load-path load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar delete-trailing-whitespace-exclude-patterns (list "\\.md$" "\\.markdown$" "\\.org$"))

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

(defun esk-pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                   nil))))))

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

;; TODO
(defun esk-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

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
  ;; (byte-recompile-directory (concat user-emacs-directory "elisp") 0)
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
	(insert candidate))))))

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


(provide 'mylib)
;;; mylib ends here

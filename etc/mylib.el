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

(provide 'mylib)
;;; mylib ends here

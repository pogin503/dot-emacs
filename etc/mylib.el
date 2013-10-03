;;; mylib --- mylib
;; This program is free software
;;; Commentary:
;;; Code:
(defun print-escaped-sexp ()
  (interactive)
  (mark-defun)
  ;; (let ((beg (region-beginning)) (end (region-end)))
  ;;   (if (<= beg end)
  ;;       (copy-to-register ?r beg end)
  ;;     (copy-to-register ?r end beg)))
  (copy-to-register ?r (region-beginning) (region-end))
  (insert (format "%S" (substring-no-properties (get-register ?r))))
  )

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
  "Insert header info."
  (interactive)
  (goto-char (point-min))
  (let ((f (file-name-base)))
    (insert (format ";;; %s --- %s\n" f f))
    (insert ";; This program is free software\n")
    (insert ";;; Commentary:\n")
    (insert ";;; Code:\n")
    (goto-char (point-max))
    (insert (format "(provide '%s)\n;;; %s ends here\n" f f))
  ))

(provide 'mylib)
;;; mylib ends here

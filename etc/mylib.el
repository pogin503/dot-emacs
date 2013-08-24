;;; mylib --- mylib
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

(defun insert-elisp-file-info ()
  (interactive)
  (goto-char (point-min))
  (let ((f (file-name-base)))
    (insert (format ";;; %s --- %s\n" f f))
    (insert ";;; Commentary:\n;;; Code:\n")
    (goto-char (point-max))
    (insert (format "(provide '%s)\n;;; %s ends here\n" f f))
  ))

(provide 'mylib)
;;; mylib ends here

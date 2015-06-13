;;; mylib-etc.el --- mylib-etc.el
;;; Commentary:
;; This program is free software
;;; Code:

(defun my-pp-for-translate ()
  (interactive)
  (save-excursion (replace-regexp "\\([a-zA-Z]\\)
" "\\1 " nil (region-beginning) (region-end))
                  ))

(defun my-pp-for-translate-quote ()
  (interactive)
  (save-excursion (replace-regexp "^> " "" nil (region-beginning) (region-end))
                  (replace-regexp "\\([a-zA-Z]\\)
" "\\1 " nil (region-beginning) (region-end))
                  (goto-char (region-beginning))
                  (insert "> ")))

(provide 'mylib-etc)
;;; mylib-etc.el ends here

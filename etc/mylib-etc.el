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
                  (insert "> "))
  )
(setq hashtable (make-hash-table :test #'equal))

(defun my-optional-test (&optional hstbl)
  (if (null hstbl)
      (progn
        (setq hstbl ex-hash)
        (message "t"))
      (message "nil")))

(defun my-gen-5w1h ()
  (interactive)
  (let ((xs '("いつ" "どこで" "だれが" "なにを" "どのように")))
    (mapc (lambda (x)
            (insert (format "%s\n" x)))
          xs)))


(provide 'mylib-etc)
;;; mylib-etc.el ends here

;;; 50_init-js --- 50_init-js
;; This program is free software
;;; Commentary:
;;; Code:

(defun my-code-golf-for-js ()
   "Thisandthat."
   (interactive)
   (mapc (lambda (x)
           (replace-regexp (concat " *\\(" x "\\) *") "\\1" nil (point-min) (point-max)))
           '("-" "=" "\\+" "\\*" "/" "<" ">" "%"))
   (replace-regexp "[ ]*\\([[:digit:]]+\\)[ ]*" "\\1" nil (point-min) (point-max))
   (replace-regexp "for " "for" nil (point-min) (point-max))
   (replace-regexp "var " "" nil (point-min) (point-max))
   (replace-regexp "{ " "{" nil (point-min) (point-max))
   (replace-regexp "^[[:space:]]* " "for" nil (point-min) (point-max))
   )

(provide '50_init-js)
;;; 50_init-js ends here

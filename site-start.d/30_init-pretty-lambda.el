;;; 30_init-pretty-lambda.el --- pretty lambda conf
;;; Commentary:
;;; Code:
(require 'pretty-lambdada)
(pretty-lambda-for-modes)
;; (defun pretty-lambdas ()
;;   "Show glyph for lower-case Greek lambda (λ) wherever 'lambda' appears."
;;   (font-lock-add-keywords
;;     nil
;;     `(("(\\(lambda\\>\\)"
;;        (0
;;          (progn
;;            (compose-region
;;              (match-beginning 1)
;;              (match-end 1)
;;              ,(make-char 'greek-iso8859-7 107))
;;            'font-lock-keyword-face))))))

;; (add-hook 'elisp-mode-hook 'pretty-lambdas)
;;; 30_init-pretty-lambda.el ends here

(defun pretty-lambdas ()
  "Show glyph for lower-case Greek lambda (λ) wherever 'lambda' appears."
  (font-lock-add-keywords
    nil
    `(("(\\(lambda\\>\\)"
       (0
         (progn
           (compose-region
             (match-beginning 1)
             (match-end 1)
             ,(make-char 'greek-iso8859-7 107))
           'font-lock-keyword-face))))))

(add-hook 'elisp-mode-hook 'pretty-lambdas)

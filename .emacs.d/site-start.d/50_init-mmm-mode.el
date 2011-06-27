;; 設定例
;;@see http://d.hatena.ne.jp/jimo1001/20071111/1194770814
;;@see http://bluestar.s32.xrea.com/text/php-mode.php
;;@see http://www.bookshelf.jp/soft/meadow_13.html#SEC102
(req mmm-mode)
(req mmm-auto)
(setq mmm-submode-decoration-level 2)
(invert-face 'mmm-default-submode-face t)
(setq mmm-font-lock-available-p t)
(setq mmm-global-mode 'maybe)
(set-face-background 'mmm-default-submode-face nil) ;背景色が不要な場合
(mmm-add-classes
 '((embedded-css
    :submode css-mode
    :front "<style[^>]*>"
    :back "</style>")))
(mmm-add-mode-ext-class nil "\\.html\\'" 'embedded-css)

;; mmm-mode in php
(mmm-add-mode-ext-class nil "\\.php?\\'" 'html-php)
(mmm-add-classes
 '((html-php
    :submode php-mode
    :front "<\\?\\(php\\)?"
    :back "\\?>")))
(add-to-list 'auto-mode-alist '("\\.php?\\'" . html-mode))


;; texinfo + lisp
(mmm-add-classes
 '((texi-elisp
    :submode emacs-lisp-mode
    :front "@lisp"
    :back "@end lisp"
    :insert ((?t texi-elisp nil @ "@lisp"
                 @ "\n" _ "\n" @ "@end lisp" @))
    )))
(mmm-add-mode-ext-class nil "\\.texi?\\'" 'texi-elisp)

(defun save-mmm-c-locals ()
    (with-temp-buffer
      (php-mode)
      (dolist (v (buffer-local-variables))
        (when (string-match "\\`c-" (symbol-name (car v)))
           (add-to-list 'mmm-save-local-variables `(,(car v) nil, mmm-c-derived-modes))))))
(save-mmm-c-locals)
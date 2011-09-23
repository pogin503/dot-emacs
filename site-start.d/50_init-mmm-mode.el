; 設定例
(add-to-list 'load-path "~/.emacs.d/elisp/mmm-mode")

(require 'mmm-auto)
(setq mmm-global-mode 'maybe)
(set-face-background 'mmm-default-submode-face nil) ;背景色が不要な場合
(mmm-add-classes
 '((embedded-css
    :submode css-mode
    :front "<style[^>]*>"
    :back "</style>")
   ))
(mmm-add-mode-ext-class nil "\\.html\\'" 'embedded-css)

;; html + php
(mmm-add-classes
 '((html-php
    :submode php-mode
    :front "<\\?\\(php\\)?"
    :back "\\?>")))
(mmm-add-mode-ext-class nil "\\.php?\\'" 'html-php)


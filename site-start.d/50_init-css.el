;;; 50_init-css-mode --- 50_init-css-mode
;; This program is free software
;;; Commentary:
;;; Code:
; 設定例
(autoload 'css-mode "css-mode")
(setq auto-mode-alist
      (cons '("\\.css$" . css-mode) auto-mode-alist))
(setq cssm-indent-function #'cssm-c-style-indenter)
(provide '50_init-css-mode)
;;; 50_init-css-mode ends here

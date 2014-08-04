;;; 50_init-makefile.el --- 50_init-makefile.el
;; This program is free software
;;; Commentary:
;;; Code:
(add-hook 'makefile-mode-hook
          (lambda ()
            (setq tab-width 8)
            (setq indent-tabs-mode t)))
(provide '50_init-makefile)
;;; 50_init-makefile.el ends here

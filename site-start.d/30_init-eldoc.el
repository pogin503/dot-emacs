;;; 30_init-eldoc --- for eldoc conf
;;; Commentary:
;;; Code:
(require 'eldoc)
(req 'eldoc-extension)
(setq eldoc-idle-delay 0.21)
(setq eldoc-echo-area-use-multiline-p t)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(provide '30_init-eldoc)
;;; 30_init-eldoc ends here

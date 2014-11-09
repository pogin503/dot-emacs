;;; 50_init-coffee-mode.el --- 50_init-coffee-mode.el
;;; Commentary:
;; This program is free software
;;; Code:

;; for coffee-mode
(defun my-coffee-mode-conf ()
  "Set coffee-mode settings."
  (message "Set coffee-mode config.")
  (custom-set-variables
   '(coffee-tab-width 2)))

(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'coffee-mode)))

(add-hook 'coffee-mode-hook 'my-coffee-mode-conf)

(provide '50_init-coffee-mode)
;;; 50_init-coffee-mode.el ends here

;;; 30_init-session.el --- for session.el conf
;;; Commentary:
;;; Code:

(if (not run-no-window)
    (progn
      (require 'session)
      (eval-when-compile
        (require '00_init-hanbetu))
      (add-hook 'after-init-hook 'session-initialize)))
;;; 30_init-session.el ends here

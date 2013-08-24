;;; 30_init-server --- 30_init-server
;;; Commentary:
;;; Code:
(require 'server)
(unless (server-running-p)
  ;; (if (< emacs-major-version 23)
  (server-start))
;; )

(provide '30_init-server)
;;; 30_init-server ends here

(require 'server)
(unless (server-running-p)
  ;; (if (< emacs-major-version 23)
  (server-start))
;; )


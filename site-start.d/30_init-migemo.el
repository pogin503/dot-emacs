;;; 30_init-migemo.el --- 30_init-migemo.el
;;; Commentary:
;; This program is free software
;;; Code:

(when (executable-find "cmigemo")
  (require 'migemo)
  (setq migemo-command "/usr/local/bin/cmigemo")
  (setq migemo-options '("-q" "--emacs" "-i" "\g"))
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (migemo-init))

(provide '30_init-migemo)
;;; 30_init-migemo.el ends here

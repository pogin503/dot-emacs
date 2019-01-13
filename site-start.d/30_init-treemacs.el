;;; 30_init-treemacs.el --- 30_init-treemacs.el -*- lexical-binding: t; coding: utf-8 -*-
;;; Commentary:
;; This program is free software
;;; Code:

(use-package treemacs
  :config
  (treemacs-git-mode 'deferred)
  (treemacs-follow-mode nil))

(require 'f)
(defun my-make-directory (dirname)
  (interactive (list (read-string
                      (format "directory name is (pwd: %s) ? "
                              default-directory))))
  (if (not (f-exists? (f-join default-directory dirname)))
      (make-directory (f-join default-directory dirname))
    (message (format "cannot make directory : %s" dirname)))
  )

(provide '30_init-treemacs)
;;; 30_init-treemacs.el ends here

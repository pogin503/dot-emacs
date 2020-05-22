;;; 30_init-treemacs.el --- 30_init-treemacs.el -*- lexical-binding: t; coding: utf-8 -*-
;;; Commentary:
;; This program is free software
;;; Code:

(require 'f)
(require 'use-package)

(use-package treemacs
  :config
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'simple))
    (`(t . _)
     (treemacs-git-mode 'simple)))
  (treemacs-follow-mode nil))

(defun my-make-directory (dirname)
  (interactive (list (read-string
                      (format "directory name is (pwd: %s) ? "
                              default-directory))))
  (if (not (f-exists? (f-join default-directory dirname)))
      (make-directory (f-join default-directory dirname))
    (message (format "cannot make directory : %s" dirname)))
  )

(defun my-set-current-buffer-treemacs ()
  (interactive)
  (pcase (treemacs-current-visibility)
    ('visible (delete-window (treemacs-get-local-window)))
    ('exists  (treemacs-select-window))
    ('none    (treemacs--init (f-this-file))))

  ;; (let ((new-root (f-this-file)))
  ;;   (treemacs-remove-project-from-workspace)
  ;;   (treemacs-do-add-project-to-workspace new-root (file-name-nondirectory new-root)
  ;;   (treemacs-goto-button new-root)
  ;;   (treemacs-toggle-node)))
  )
(provide '30_init-treemacs)
;;; 30_init-treemacs.el ends here

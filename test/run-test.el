;;; run-test.el --- for test file
;;; Commentary:
;;; Code:

;; (setq user-emacs-directory default-directory)

;; (message user-emacs-directory)

(defun add-to-load-path (&rest paths)
  "Add to load path recursively.
`PATHS' Directorys you want to read recursively."
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; Utils
(defun myfile:test-join-path (path &rest rest)
  "Join a list of PATHS with appropriate separator (such as /).

\(fn &rest paths)"
  (if rest
      (concat (file-name-as-directory path) (apply 'myfile:test-join-path rest))
    path))

(defconst myfile:test-dir
  (if load-file-name
      (file-name-directory load-file-name)
    ;; Fall back to default directory (in case of M-x eval-buffer)
    default-directory)
  "Directory of the test suite.")

(defconst myfile:root-dir (expand-file-name (concat myfile:test-dir "..")))

;; Setup `load-path'
(mapc (lambda (p) (add-to-list 'load-path p))
      (list myfile:test-dir
            myfile:root-dir))

(add-to-load-path
 "site-start.d"
 "plugins"
 "elpa"
 "elisp"
 "plugins"
 "etc"
 "test"
 ".cask")

(provide 'run-test)
;;; run-test ends here

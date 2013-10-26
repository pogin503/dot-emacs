;;; run-test.el --- for test file

;;; Commentary:

;;; Code:
(defun add-to-load-path (&rest paths)
  "Add to load path recursively.
`PATHS' Directory you want to read recursively."
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-loadpath)
            (normal-top-level-add-subdirs-to-load-path))))))


(add-to-load-path
 "site-start.d"
 "plugins"
 "elpa"
 "elisp"
 "plugins"
 "etc")

;;; run-test ends here

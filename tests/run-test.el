;;; run-test.el --- for test file

;;; Commentary:

;;; Code:
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-loadpath)
            (normal-top-level-add-subdirs-to-load-path))))))


(add-to-load-path
 "site-start.d"
 "plugins")

;;; run-test ends here

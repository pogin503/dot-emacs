;;; 50_init-agda.el --- 50_init-agda.el
;; Author: Ryo
;; Version:
;; Package-Requires: ()
;;; Commentary:
;; This program is free software
;;; Code:

;; agda-mode
(load-file
 (let ((coding-system-for-read 'utf-8))
   (shell-command-to-string "agda-mode locate")))


(provide '50_init-agda)
;;; 50_init-agda.el ends here

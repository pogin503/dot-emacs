;;; 50_init-agda.el --- 50_init-agda.el
;;; Commentary:
;;
;; This program is free software
;;
;; Install Agda
;;
;; cabal update
;; cabal install Agda
;; agda-mode setup
;;
;; (load-file (let ((coding-system-for-read 'utf-8))
;;              (shell-command-to-string "agda-mode locate")))
;;
;; agda-mode compile
;;
;;; Code:

(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))

(provide '50_init-agda)
;;; 50_init-agda.el ends here

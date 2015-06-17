;;; 00_init-el-get --- 00_init-el-get
;; This program is free software
;;; Commentary:
;;; Code:
;; (setq el-get-dir "~/.emacs.d/elisp/el-get/")
;; So the idea is that you copy/paste this code into your *scratch* buffer,
;; hit C-j, and you have a working developper edition of el-get.

(add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path (concat user-emacs-directory "el-get-user/recipes"))
(el-get 'sync)

(provide '00_init-el-get)
;;; 00_init-el-get ends here

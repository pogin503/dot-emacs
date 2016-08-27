;;; 00_init-el-get --- 00_init-el-get
;; This program is free software
;;; Commentary:
;;; Code:
;; (setq el-get-dir "~/.emacs.d/elisp/el-get/")
;; So the idea is that you copy/paste this code into your *scratch* buffer,
;; hit C-j, and you have a working developper edition of el-get.

(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path (locate-user-emacs-file "el-get-user/recipes"))
(el-get-bundle tarao/with-eval-after-load-feature-el)
(el-get-bundle syohex/emacs-jazzradio)
(el-get-bundle k1LoW/emacs-historyf)

(el-get 'sync)

(provide '00_init-el-get)
;;; 00_init-el-get ends here

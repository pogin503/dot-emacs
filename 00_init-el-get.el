;;; 00_init-el-get --- 00_init-el-get
;; This program is free software
;;; Commentary:
;;; Code:
;; (setq el-get-dir "~/.emacs.d/elisp/el-get/")
;; So the idea is that you copy/paste this code into your *scratch* buffer,
;; hit C-j, and you have a working developper edition of el-get.

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

;; レシピ置き場
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get/recipes")

;; 追加のレシピ置き場
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get/local-recipes")

(el-get 'sync)

(setq el-get-verbose t)

;; personal recipes
;; (setq el-get-sources nil)

;; (setq dim-packages
;;       (append
;;        ;; list of packages we use straight from official recipes
;;        '(
;;          ;; gnus
;;          ;; bbdb switch-window vkill google-maps pgdevenv-el
;;          ;; mbsync asciidoc smex geiser xcscope multiple-cursors
;;          ;; anything descbinds-anything pcmpl-git magit-view-file
;;          ;; emacs-goodies-el sicp auto-dictionnary keywiz pandoc-mode
;;          ;; pgsql-linum-format psvn rect-mark crontab-mode icomplete+
;;          ;; php-mode-improved rainbow-delimiters muse deft dpans2texi
;;          ;; markdown-mode color-theme-solarized protobuf-mode paredit
;;          hatena-diary
;;          )

;;          (mapcar 'el-get-as-symbol (mapcar 'el-get-source-name el-get-sources))))

;; (el-get 'sync dim-packages)
(provide '00_init-el-get)
;;; 00_init-el-get ends here

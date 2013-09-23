;;; 00_init-el-get --- 00_init-el-get
;;; Commentary:
;;; Code:
;; (setq el-get-dir "~/.emacs.d/elisp/el-get/")
(setq el-get-dir "~/.emacs.d/elisp/el-get/")
(url-retrieve
 "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
 (lambda (s)
   (goto-char (point-max))
   (eval-print-last-sexp)))

(setq el-get-dir "~/.emacs.d/elisp/el-get/")

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(el-get 'sync)

;; (unless (require 'el-get nil 'noerror)
;;   (with-current-buffer
;;       (url-retrieve-synchronously
;;        "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
;;     (goto-char (point-max))
;;     (eval-print-last-sexp)))

;; レシピ置き場
(add-to-list 'el-get-recipe-path
             (concat user-emacs-directory "elisp/el-get/recipes"))
(setq el-get-verbose t)


;; 追加のレシピ置き場
(add-to-list 'el-get-recipe-path
             "~/.emacs.d/el-get/local-recipes")
;; (setq el-get-user-package-directory "~/.emacs.d/elisp/el-get/local-recipes")

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

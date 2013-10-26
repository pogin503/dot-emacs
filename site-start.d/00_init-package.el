;;; 00_init-package.el --- package conf
;;; Commentary:
;;; Code:
(require 'package)

;;リポジトリにMarmaladeを追加
(add-to-list 'package-archives
			   '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives
;; 			 '("tromy" . "http://tromey.com/elpa/"))
;; (add-to-list 'package-archives
;; 			   '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;;(pop package-archives)
;;インストールするディレクトリを指定
(setq package-user-dir (concat user-emacs-directory "elpa"))

;;インストールしたパッケージにロードパスを通してロードする
(package-initialize)

;; install packages by package.el
;; (mapc
;;  (lambda (package)
;;    (when (not (package-installed-p package))
;;      (package-install package)))

(defconst installing-package-list
  '(
   ;; starter-kit
   auto-complete
   auto-install
   clojure-mode
   coffee-mode
   color-moccur
   dash
   deferred
   direx
   e2wm
   eldoc-extension
   flycheck
   git-gutter
   haskell-mode
   helm
   helm-anything
   helm-descbinds
   ;; helm-c-moccur
   helm-ls-git
   ht
   inf-ruby
   js2-mode
   magit
   markdown-mode
   ;; mmm-mode
   multi-web-mode
   multiple-cursors
   nginx-mode
   org-bullets
   php-mode
   popwin
   powerline
   pretty-lambdada
   quickrun
   redo+
   rhtml-mode
   rinari
   ruby-end
   s
   session
   slime
   solarized-theme
   starter-kit-ruby
   yasnippet
   helm-c-yasnippet
   ))
(eval-when-compile
  (require 'cl))

(require 'melpa)


(defun my-install-package ()
  "Install my package."
  (let ((not-installed (loop for x in installing-package-list
                            when (not (package-installed-p x))
                            collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
        (package-install pkg)))))

;;; 00_init-package.el ends here

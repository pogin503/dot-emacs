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
   emmet-mode
   f
   flycheck
   git-gutter
   haskell-mode
   helm
   helm-anything
   helm-c-yasnippet
   helm-descbinds
   helm-emmet
   ;; helm-c-moccur
   helm-ls-git
   ht
   inf-ruby
   js2-mode
   magit
   markdown-mode
   ;; mmm-mode
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
   ruby-electric
   ruby-end
   s
   session
   slime
   solarized-theme
   web-mode
   yasnippet
   ))

(eval-when-compile
  (require 'cl))

(require 'melpa)

(defun my-install-package ()
  "Install my packages."
  (let ((not-installed (loop for x in installing-package-list
                             when (not (package-installed-p x))
                             collect x)))
    (when not-installed
      (package-refresh-contents)
      (dolist (pkg not-installed)
        (package-install pkg))))
  (message "done")
  )

(my-install-package)

;; cask setting

;; .cask
;; cask init
(add-to-list 'auto-mode-alist '("Cask$" . emacs-lisp-mode))
(require 'cask "~/.cask/cask.el")
(cask-initialize)


;;; 00_init-package.el ends here

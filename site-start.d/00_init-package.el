;;; 00_init-package.el --- package conf
;;; Commentary:
;;; Code:
(require 'package)

;; .cask
;; cask init
(add-to-list 'auto-mode-alist '("Cask$" . emacs-lisp-mode))
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;;リポジトリにMarmaladeを追加
;; (add-to-list 'package-archives
;; 			 '("tromy" . "http://tromey.com/elpa/"))
;; (add-to-list 'package-archives
;; 			   '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(add-to-list 'package-archives
'("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)

;; ;;インストールするディレクトリを指定
;; (setq package-user-dir (concat user-emacs-directory "elpa"))

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
   expand-region
   f
   flycheck
   git-gutter
   haml-mode
   haskell-mode
   helm
   helm-anything
   helm-c-yasnippet
   helm-descbinds
   helm-emmet
   ;; helm-c-moccur
   helm-gtags
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

(require 'melpa)

;; (require 'mylib)
;; (my-install-package installing-package-list)

;; cask setting

;; .cask
;; cask init
(add-to-list 'auto-mode-alist '("Cask$" . emacs-lisp-mode))
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;;; 00_init-package.el ends here

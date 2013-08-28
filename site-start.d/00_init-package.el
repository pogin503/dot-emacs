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

(require 'melpa)

;; install packages by package.el
(mapc
 (lambda (package)
   (when (not (package-installed-p package))
     (package-install package)))
 '(
   ;; starter-kit
   auto-complete
   auto-install
   clojure-mode
   coffee-mode
   color-moccur
   dash
   e2wm
   flycheck
   git-gutter
   haskell-mode
   helm
   helm-anything
   helm-c-moccur
   helm-c-yasnippet
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
   rhtml-mode
   rinari
   ruby-end
   s
   ;; session
   slime
   solarized-theme
   starter-kit-ruby
   yasnippet
   ))

;;; 00_init-package.el ends here

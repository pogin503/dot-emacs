(require 'package)

;;リポジトリにMarmaladeを追加
(add-to-list 'package-archives
			   '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives
;; 			 '("tromy" . "http://tromey.com/elpa/"))
;; (add-to-list 'package-archives
;; 			   '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

;;(pop package-archives)
;;インストールするディレクトリを指定
(setq package-user-dir (concat user-emacs-directory "elpa"))

;;インストールしたパッケージにロードパスを通してロードする
(package-initialize)
;; install packages by package.el
(mapc
 (lambda (package)
   (or (package-installed-p package)
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
   flymake-easy
   git-gutter
   haskell-mode
   helm
   helm-anything
   helm-c-moccur
   helm-c-yasnippet
   helm-ls-git
   ht
   js2-mode
   magit
   markdown-mode
   mmm-mode
   multiple-cursors
   nginx-mode
   org
   php-mode
   popwin
   powerline
   pretty-lambdada
   quickrun
   rhtml-mode
   ruby-end
   s
   session
   slime
   solarized-theme
   starter-kit-ruby
   yasnippet
   ))

(require 'melpa)

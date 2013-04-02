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

(require 'melpa)

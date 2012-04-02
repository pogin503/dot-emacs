(require 'package)

;;リポジトリにMarmaladeを追加
(add-to-list 'package-archives
			 ;; '(("tromy" . "http://tromey.com/elpa/")
			 ;;   ("gnu" . "http://elpa.gnu.org/packages/")
			   '("marmalade" . "http://marmalade-repo.org/packages/"))
;;(pop package-archives)
;;インストールするディレクトリを指定
(setq package-user-dir (concat user-emacs-directory "elpa"))

;;インストールしたパッケージにロードパスを通してロードする
(package-initialize)

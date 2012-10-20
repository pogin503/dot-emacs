;; メニューバー日本語化
;; http://www11.atwiki.jp/s-irie/pages/13.html
(if (and (= emacs-major-version 22)
	 (eq window-system 'x))
    (setq menu-tree-coding-system 'utf-8))
(require 'menu-tree)

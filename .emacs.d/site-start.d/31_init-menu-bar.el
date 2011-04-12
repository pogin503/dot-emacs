(if (and (= emacs-major-version 22)
	 (eq window-system 'x))
    (setq menu-tree-coding-system 'utf-8))
(require 'menu-tree)

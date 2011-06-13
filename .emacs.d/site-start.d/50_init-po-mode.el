(add-to-list 'load-path "~/.emacs.d/plugins/gettext")

(setq auto-mode-alist
	  (cons '("\\.po\\'\\|\\.po\\." . po-mode) auto-mode-alist))
(autoload-if-found 'po-mode "po-mode" "Major mode for translators to edit PO files" t)
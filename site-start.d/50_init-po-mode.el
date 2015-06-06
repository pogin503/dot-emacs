(add-to-list 'load-path (concat user-emacs-directory "plugins/gettext"))

(setq auto-mode-alist
	  (cons '("\\.po\\'\\|\\.po\\." . po-mode) auto-mode-alist))
(autoload-if-found 'po-mode "po-mode" "Major mode for translators to edit PO files" t)

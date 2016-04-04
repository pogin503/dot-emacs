;;; 50_init-po.el --- 50_init-po.el -*- lexical-binding: t; coding: utf-8 -*-
;;; Commentary:
;; This program is free software
;;; Code:

(add-to-list 'load-path (locate-user-emacs-file "plugins/gettext"))

(setq auto-mode-alist
	  (cons '("\\.po\\'\\|\\.po\\." . po-mode) auto-mode-alist))
(autoload-if-found 'po-mode "po-mode" "Major mode for translators to edit PO files" t)

(provide '50_init-po)
;;; 50_init-po.el ends here

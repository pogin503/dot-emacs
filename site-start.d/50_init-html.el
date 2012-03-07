;; @see http://d.hatena.ne.jp/tototoshi/20110127/1296132523

(add-to-load-path "plugins/zencoding")
(req zencoding-mode)
(dolist (hook (list
			   'nxml-mode-hook
			   'sgml-mode-hook
			   'html-mode-hook
			   ))
  (add-hook hook 'zencoding-mode))
;; (add-hook 'text-mode-hook 'zencoding-mode)
(define-key zencoding-mode-keymap (kbd "C-c C-m") 'zencoding-expand-line)
(define-key zencoding-preview-keymap (kbd "C-c C-m") 'zencoding-preview-accept)
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
				  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
				  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)

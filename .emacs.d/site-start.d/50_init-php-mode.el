(add-to-list 'load-path "~/.emacs.d/plugins/php-mode/")
(autoload 'php-mode "php-mode")
(req php-mode)
(setq auto-mode-alist
      (cons '("\\.php\\'" . php-mode) auto-mode-alist))
(setq php-mode-force-pear t)
(add-hook 'php-mode-user-hook
  '(lambda ()
     ;;(setq php-manual-path "/usr/local/share/php/doc/html")
     (setq php-manual-url "http://www.phppro.jp/phpmanual/")))


(add-to-list 'load-path "~/.emacs.d/plugins/php-mode/")
(autoload-if-found 'php-mode "php-mode")
(setq auto-mode-alist
      (cons '("\\.php\\'" . php-mode) auto-mode-alist))
(lazyload (php-mode) "php-mode"
          (req php-mode)
          (require 'php-completion)
          (php-completion-mode t)
          (define-key php-mode-map (kbd "C-o") 'phpcmp-complete)
          (when (require 'auto-complete nil t)
            (make-variable-buffer-local 'ac-sources)
            (add-to-list 'ac-sources 'ac-source-php-completion)
            (auto-complete-mode t))
          ;; (define-key php-mode-map "C-l" 'php-complete-function)
          (define-key php-mode-map "C-m" 'newline-and-indent)
          ;; (setq php-manual-path "/usr/share/doc/php/html")
          (setq php-manual-url "http://www.phppro.jp/phpmanual"))

(add-hook-fn 'php-mode-hook
             (setq tab-width 4)
             (setq c-basic-offset 4)
             (setq indent-tabs-mode nil)
             (setq php-mode-force-pear t))

;;; 50_init-html --- 50_init-html
;;; Commentary:
;;; Code:
;; @see http://d.hatena.ne.jp/tototoshi/20110127/1296132523
(require '00_init-macro)

;; (add-to-load-path "plugins/zencoding")
(req web-mode

;;; emacs 23以下の互換
     ;; (when (< emacs-major-version 24)
     ;;   (defalias 'prog-mode 'fundamental-mode))

     ;;; 適用する拡張子
     (add-to-list 'auto-mode-alist '("\\.phtml$"     . web-mode))
     (add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
     (add-to-list 'auto-mode-alist '("\\.jsp$"       . web-mode))
     (add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . web-mode))
     (add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
     (add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))
     ;;; インデント数
     (defun web-mode-hook ()
       "Hooks for Web mode."
       (setq web-mode-html-offset   2)
       (setq web-mode-css-offset    2)
       (setq web-mode-script-offset 2)
       (setq web-mode-php-offset    2)
       (setq web-mode-java-offset   2)
       (setq web-mode-asp-offset    2)
     ))



(add-hook 'web-mode-hook 'web-mode-hook)

(req emmet-mode
     (define-key emmet-mode-keymap (kbd "C-j") nil) ;; C-j は newline のままにしておく
     (keyboard-translate ?\C-i ?\H-i) ;;C-i と Tabの被りを回避
     (define-key emmet-mode-keymap (kbd "H-i") 'emmet-expand-line) ;; C-i で展開)
     )
(add-hook 'sgml-mode-hook 'emmet-mode) ;; マークアップ言語全部で使う
(add-hook 'css-mode-hook  'emmet-mode) ;; CSSにも使う
(add-hook 'emmet-mode-hook
          (lambda () (setq emmet-indentation 2))) ;; indent はスペース2個

(req multi-web-mode
     (setq mweb-default-major-mode 'html-mode)
     (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                       (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                       (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
     (setq mweb-filename-extensions '("htm" "html" "ctp" "phtml"))
     (multi-web-global-mode 1))

(provide '50_init-html)
;;; 50_init-html ends here

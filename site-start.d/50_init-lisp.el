(add-hook 'lisp-mode-hook
          (lambda ()
            (show-paren-mode t)))
(add-hook 'after-change-major-mode-hook
          (lambda ()
            (show-paren-mode t)))
(add-hook 'lisp-mode-hook
          (lambda ()
            (set (make-local-variable 'lisp-indent-function)
                 'common-lisp-indent-function)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ;; if文でEmacsLispのインデントをCommonLispのインデントに変える設定
            ;; (set (make-local-variable 'lisp-indent-function)
            ;;  'common-lisp-indent-function)
            (show-paren-mode t)))

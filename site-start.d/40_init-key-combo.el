;;; 40_init-key-combo --- 40_init-key-combo
;; This program is free software
;;; Commentary:
;;; Code:

(require 'key-combo)

(key-combo-mode 1)

(key-combo-load-default)

(require 'mylib-keycombo)

(my-key-combo-global-conf)

(add-hook 'js2-mode-hook 'my-key-combo-js-conf)
(add-hook 'javascript-mode-hook 'my-key-combo-js-conf)

(add-hook 'c++-mode-hook 'my-c++-mode-key-combo)

(add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-key-combo)

(add-hook 'lisp-interaction-mode-hook 'my-lisp-mode-key-combo)

;; (key-combo-define-global (kbd "(") "(`!!')" ")")
;; (key-combo-define-global (kbd "\"") "(`!!')" "\"")
;; (key-combo-define-global KEYS COMMAND &optional GUARD)

;; @see http://d.hatena.ne.jp/marony0607/20111205/1323103005
;; ここの表を参考にいくつかkey-comboを作った。

(add-hook 'haskell-mode-hook 'my-key-combo-haskell-conf)

;; (add-hook 'sh-mode-hook
;;           #'(lambda ()
;;               (key-combo-define-local (kbd "=") "=")
;;               ))

;; (add-hook 'coq-mode-hook 'my-coq-mode-key-combo)
;; (add-hook 'proof-activate-scripting-hook 'my-coq-mode-key-combo)
;; (dont-compile
;;   (when (fboundp 'describe)
;;     (describe ("align test in temp-buffer" :vars ((mode)))
;;       ())))

(add-hook 'prolog-mode-hook 'my-prolog-mode-key-combo)

(add-hook 'coffee-mode-hook 'my-coffee-mode-key-combo)

(add-hook 'ruby-mode-hook 'my-ruby-mode-key-combo)

(add-hook 'makefile-mode-hook 'my-makefile-key-combo-conf)

(provide '40_init-key-combo)
;;; 40_init-key-combo ends here

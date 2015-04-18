;;; 40_init-flex-autopair --- 40_init-flex-autopair
;; This program is free software
;;; Commentary:
;;; Code:
(require 'flex-autopair)
(flex-autopair-mode 1)

(require 'mylib)

(add-hook 'haskell-mode-hook 'my-haskell-hook-function1)

(add-hook 'sh-mode-hook 'my-flex-sh-pair)

(setq flex-autopair-user-conditions-high
      `(
        ((and
          (eq last-command-event ?')
          ;; カーソルの後ろが空白以外ならばシングルクオート自身を挿入する
          (save-excursion (re-search-backward "[^ ]" (- (point) 1) t))
          (memq major-mode flex-autopair-functional-modes))
         . self)
        ((and
          (eq major-mode 'c-mode)
          (eq last-command-event ?<)
          (save-excursion
            (re-search-backward "#include\\|template\\|vector" (point-at-bol) t)))
         . pair)
        ((and
          (eq major-mode 'sh-mode)
          (eq last-command-event ?[ )
          (flex-autopair-execute-macro
           (format "%c `!!' %c" opener closer)))
         . pair-in-two-spaces)
        ))


(flex-autopair-reload-conditions)
(provide '40_init-flex-autopair)
;;; 40_init-flex-autopair ends here

;;; 30_init-align --- 30_init-align
;;; Commentary:
;;; Code:
(require 'align)

;; Align for haskell-mode
(add-to-list 'align-rules-list
             `(haskell-right-arrow
               (regexp . ,(concat "\\(\\s-*\\)->\\(\\s-*\\)"
                                 "\\([^= \t\n]\\|$\\)"))
               (group . (1 2))
               (justify . t)
               ;; (repeat . t)
               ;; (tab-stop . nil)
               (modes . '(haskell-mode))))
(add-to-list 'align-rules-list
             `(haskell-left-arrow
               (regexp . ,(concat "\\(\\s-*\\)<-\\(\\s-*\\)"
                                 "\\([^= \t\n]\\|$\\)"))
               (group . (1 2))
               (justify . t)
               ;; (repeat . t)
               ;; (tab-stop . nil)
               (modes . '(haskell-mode))))
(add-to-list 'align-rules-list
             '(haskell-assignment
               (regexp . "\\(\\s-*\\)=\\(\\s-*\\)[a-zA-Z]")
               (group . (1 2))
               (justify . t)
               (repeat . t)
               (tab-stop . nil)
               (modes . '(haskell-mode))))
(add-to-list 'align-rules-list
             '(haskell-def-operator
               (regexp . "[a-zA-z']\\(\\s-*\\)::\\(\\s-*\\)")
               (group . (1 2))
               (modes . '(haskell-mode))))

;; (pop align-rules-list)

(provide '30_init-align)
;;; 30_init-align ends here

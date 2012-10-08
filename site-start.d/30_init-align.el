(req align)

;; ;; Align for php-mode
;; ;; http://d.hatena.ne.jp/Tetsujin/20070614/1181757931
;; (add-to-list 'align-rules-list
;;              '(php-assignment
;;                (regexp   . "[^-=!^&*+<>/.| \t\n]\\(\\s-*[.-=!^&*+<>/|]*\\)=>?\\(\\s-*\\)\\([^= \t\n]\\|$\\)")
;;                (justify  . t)
;;                (tab-stop . nil)
;;                (modes    . '(php-mode))))
;; (add-to-list 'align-dq-string-modes 'php-mode)
;; (add-to-list 'align-sq-string-modes 'php-mode)
;; (add-to-list 'align-open-comment-modes 'php-mode)
;; (setq align-region-separate (concat "\\(^\\s-*$\\)\\|"
;;                                     "\\([({}\\(/\*\\)]$\\)\\|"
;;                                     "\\(^\\s-*[)}\\(\*/\\)][,;]?$\\)\\|"
;;                                     "\\(^\\s-*\\(}\\|for\\|while\\|if\\|else\\|"
;;                                     "switch\\|case\\|break\\|continue\\|do\\)[ ;]\\)"
;; 									))

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

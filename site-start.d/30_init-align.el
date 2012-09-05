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
             '(haskell-arrow
               (regexp . "\\(\\s-*\\)->\\(\\s-*\\)")
               (repeat . t)
               (modes . '(haskell-mode))))
(add-to-list 'align-rules-list
             '(haskell-bind
               (regexp . "\\(\\s-*\\)=\\(\\s-*\\)")
               (repeat . t)
               (modes . '(haskell-mode))))

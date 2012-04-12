(require 'flex-autopair)
(flex-autopair-mode 1)


;; (defun my-hook-function ()
;;   (add-to-list 'flex-autopair-pairs '(?\< . ?\>)))

;; (add-hook 'c-mode-hook 'my-hook-function)
;; (add-hook 'c++-mode-hook 'my-hook-function)

;; (defun my-haskell-hook-function1 ()
;;   (add-to-list 'flex-autopair-pairs '(?\' . ?\')))

;; (add-hook 'haskell-mode-hook 'my-haskell-hook-function1)

;; (add-to-list 'flex-autopair-user-conditions-high
;;              '((openp . \')
;;                (closep . \')))
;; (add-to-list 'flex-autopair-user-conditions-high
;;              '((openp . \`)
;;                (closep . \`)))

(setq flex-autopair-user-conditions-high
      `(((and
          (eq major-mode 'c-mode)
          (eq last-command-event ?<)
          (save-excursion
            (re-search-backward "#include" (point-at-bol) t)))
         . pair)
        ;; ((and
        ;;   (eq major-mode 'c-mode)
        ;;   (eq last-command-event ?<))
        ;;  . self)
        ((and
           (eq major-mode 'c++-mode)
           (eq last-command-event ?<)
           (save-excursion
             (re-search-backward "#include" (point-at-bol) t)))
          . pair)
        ;; ((and
        ;;    (eq major-mode 'c++-mode)
        ;;    (eq last-command-event ?<)
        ;;    (save-excursion
        ;;      (re-search-backward "template" (point-at-bol) t)))
        ;;   . pair)
        ;; ((and
        ;;    (eq major-mode 'haskell-mode)
        ;;    (eq last-command-event ?'))
        ;;   . pair)

        ((and
          (eq major-mode 'c++-mode)
          (eq last-command-event ?<))
         . self)
        ;; ((and
        ;;   (eq major-mode 'c++-mode)
        ;;   (eq last-command-event ?>)
        ;;   (save-excursion
        ;;     (re-search-backward "<" (point-at-bol) t)))
        ;;  . self)
        ))

(flex-autopair-reload-conditions)

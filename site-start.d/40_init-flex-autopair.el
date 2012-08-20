(require 'flex-autopair)
(flex-autopair-mode 1)


;; (defun my-hook-function ()
;;   (add-to-list 'flex-autopair-pairs '(?\< . ?\>)))

;; (add-hook 'c-mode-hook 'my-hook-function)
;; (add-hook 'c++-mode-hook 'my-hook-function)

(defun my-haskell-hook-function1 ()
  (add-to-list 'flex-autopair-pairs '(?\' . ?\')))

(add-hook 'haskell-mode-hook 'my-haskell-hook-function1)

;; (add-to-list 'flex-autopair-user-conditions-high
;;              '(
;;                ;; (openp . \')
;;                ;; (closep . \')
;;                (pair-in-two-spaces . (flex-autopair-execute-macro
;;                                       (format "c% `!!' %c" opener closer)))
;;                ))
;; (add-to-list 'flex-autopair-user-conditions-high
;;              '((openp . \`)
;;                (closep . \`)))
(defun my-flex-sh-pair ()
  (add-to-list 'flex-autopair-pairs '(?\[ . ?\])))

(add-hook 'sh-mode-hook 'my-flex-sh-pair)


(setq flex-autopair-user-conditions-high
      `(
        ((and
          (eq last-command-event ?')
          ;; (if (numberp (save-excursion (re-search-backward "[^ ]" (- (point) 1) t))) t nil)
          ;; カーソルの後ろが空白以外ならばシングルクオート自身を挿入する
          (save-excursion (re-search-backward "[^ ]" (- (point) 1) t))
          (memq major-mode flex-autopair-functional-modes))
         . self)
;;      ((and
;;           (eq major-mode 'c-mode)
;;           (eq last-command-event ?<)
;;           (save-excursion
;;             (re-search-backward "#include\\|template" (point-at-bol) t)))
;;          . pair)
;;         ;; ((and
;;         ;;   (eq major-mode 'c-mode)
;;         ;;   (eq last-command-event ?<))
;;         ;;  . self)
;;         ((and
;;            (eq major-mode 'c++-mode)
;;            (eq last-command-event ?<)
;;            (save-excursion
;;              (re-search-backward "#include\\|" (point-at-bol) t)))
;;           . pair)
;;         ;; ((and
;;         ;;    (eq major-mode 'c++-mode)
;;         ;;    (eq last-command-event ?<)
;;         ;;    (save-excursion
;;         ;;      (re-search-backward "template" (point-at-bol) t)))
;;         ;;   . pair)
;;         ;; ((and
;;         ;;    (eq major-mode 'haskell-mode)
;;         ;;    (eq last-command-event ?'))
;;         ;;   . pair)

;;         ((and
;;           (eq major-mode 'c++-mode)
;;           (eq last-command-event ?<))
;;          . self)
;;         ;; ((and
;;         ;;   (eq major-mode 'c++-mode)
;;         ;;   (eq last-command-event ?>)
;;         ;;   (save-excursion
;;         ;;     (re-search-backward "<" (point-at-bol) t)))
;;         ;;  . self)
        ((and
          (eq major-mode 'sh-mode)
          (eq last-command-event ?[ )
          (flex-autopair-execute-macro
           (format "%c `!!' %c" opener closer)))
         . pair-in-two-spaces)
        ))



(flex-autopair-reload-conditions)

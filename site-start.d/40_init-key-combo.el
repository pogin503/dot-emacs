(require 'key-combo)
(key-combo-load-default)
;; (key-combo-define-global (kbd "\"") "\"`!!'\"")
;; (key-combo-define-global (kbd "\"\"") "\"\"`!!'")
;; (key-combo-define-global (kbd "(") "(`!!')")
;; (key-combo-define-global (kbd "()") "(`!!')")
;; (key-combo-define-global (kbd "`") '("``!!''" "`"))
;; (key-combo-define-global (kbd "'") "'`!!''")
;; (key-combo-define-global (kbd "''") "'`!!''")

(add-hook 'c++-mode-hook
          #'(lambda ()
              ;; (key-combo-define-local (kbd "<>") "<`!!'>")
              ;; (key-combo-define-local (kbd "[") "[`!!']")
              ;; (key-combo-define-local (kbd "[]") "[]`!!'")
              ;; (key-combo-define-local (kbd "()") "()`!!'")
              (key-combo-define-local (kbd "//") "// ")
              (key-combo-define-local (kbd "/**/") "/* `!!' */")
              (key-combo-define-local (kbd "//") "// ")
              ;; (key-combo-define-local (kbd ">") '(key-combo-execute-orignal " >> "))
              ;; (key-combo-define-local (kbd "<") '(" < " " << "))
              ))

(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              (key-combo-define-local (kbd ";") 'key-combo-execute-orignal)
              (key-combo-define-local (kbd ";=") ";=> ")
              (key-combo-define-local (kbd "\\#'") "\\#'()")
              ;; (key-combo-define-local (kbd "\\#'") "\\#'()")
              ;; (key-combo-define-local (kbd "()") "(`!!')"))
              ;; (key-combo-define-local (kbd "''") "'`!!''"))
              ;; (key-combo-define-local (kbd "\"\"") "[`!!']"))
              ;; (key-combo-define-local (kbd "[]") "[`!!']"))
              ))
(add-hook 'lisp-interaction-mode-hook
          #'(lambda ()
              (key-combo-define-local (kbd ";=") ";=> ")
              (key-combo-define-local (kbd "\\#'") "\\#'()")
              ))

;; (key-combo-define-global (kbd "(") "(`!!')" ")")
;; (key-combo-define-global (kbd "\"") "(`!!')" "\"")
;; (key-combo-define-global KEYS COMMAND &optional GUARD)

(add-hook 'haskell-mode-hook
          #'(lambda ()
              ;; (key-combo-define-local (kbd "--") "-- ")
              (key-combo-define-local (kbd "," ) ", ")
              (key-combo-define-local (kbd "::" ) " :: ")
              (key-combo-define-local (kbd "+") '(" + " " ++ "))
              ;; (key-combo-define-local (kbd "-") 'key-combo-execute-orignal)
              ;; (key-combo-define-local (kbd "<") 'key-combo-execute-orignal)
              ;; (key-combo-define-local (kbd "->") " -> ")
              ;; (key-combo-define-local (kbd "<-") " <- ")
              ))


;; (add-hook 'sh-mode-hook
;;           #'(lambda ()
;;               (key-combo-define-local (kbd "=") "=")
;;               ))

(add-hook 'makefile-mode-hook
          #'(lambda ()
              (key-combo-define-local (kbd "$") '("$" "$(`!!')"))
              (key-combo-define-local (kbd ":") ": ")
              ))

(require 'key-combo)

(key-combo-load-default)
;; (key-combo-define-global (kbd "\"") "\"`!!'\"")
;; (key-combo-define-global (kbd "\"\"") "\"\"`!!'")
;; (key-combo-define-global (kbd "(") "(`!!')")
;; (key-combo-define-global (kbd "()") "(`!!')")
;; (key-combo-define-global (kbd "`") '("``!!''" "`"))
;; (key-combo-define-global (kbd "'") "'`!!''")
;; (key-combo-define-global (kbd "''") "'`!!''")
(key-combo-define-global (kbd "@see") "@see ")


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
              ;; (key-combo-define-local (kbd ";") 'key-combo-execute-orignal)
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

;; @see http://d.hatena.ne.jp/marony0607/20111205/1323103005
;; ここの表を参考にいくつかkey-comboを作った。
(add-hook 'haskell-mode-hook
          #'(lambda ()
              (key-combo-define-local (kbd "," ) ", ")
              ;; bind function, Eq
              (key-combo-define-local (kbd "=" ) '(" = " " == "))
              (key-combo-define-local (kbd "::" ) " :: ")
              ;; Num, List, Arrow
              (key-combo-define-local (kbd "+") '(" + " " ++ " " +++ "))

              ;; Num, comment
              (key-combo-define-local (kbd "-") (" - " "-- "))
              ;; comment
              (key-combo-define-local (kbd "{-") "{- `!!' -}")
              ;; これはまだhaskell-mode1が対応していないようなので無理。
              ;; (key-combo-define-local (kbd "{- RET") "{-\n`!!'\n-}")

              (key-combo-define-local (kbd "->") " -> ")
              (key-combo-define-local (kbd "<-") " <- ")
              (key-combo-define-local (kbd "=>") " => ")

              ;; Ord 
              (key-combo-define-local (kbd ">") " > ") ;; Ord(>) or Sequence >
              (key-combo-define-local (kbd ">=") " >= ")
              (key-combo-define-local (kbd "<") " < ") ;; Ord (<) or Sequence <
              (key-combo-define-local (kbd "<=") " <= ")

              ;; Fractional, Array
              (key-combo-define-local (kbd "/") '(" / " " // "))

              ;; pattern-match, Bool, Arrow
              (key-combo-define-local (kbd "|") '(" | " " || " " ||| "))
              ;; (key-combo-define-local (kbd "&") '(" & " " && "))
              ;; List
              (key-combo-define-local (kbd "!!") " !! ")
              ;; Eq
              (key-combo-define-local (kbd "\=") " \= ")

              ;; Applicative
              (key-combo-define-local (kbd "<*>") " <*> ") ;; Applicative (<*>) or GraphRep <*
              (key-combo-define-local (kbd "<*") " <* ")
              (key-combo-define-local (kbd "*>") " *> ")
              (key-combo-define-local (kbd "<**>") " <**> ")

              ;; Alternative
              (key-combo-define-local (kbd "<|") " <|> ")

              ;; Functor
              (key-combo-define-local (kbd "<$") " <$ ")
              (key-combo-define-local (kbd "<$>") " <$> ")
              
              ;; Monad
              (key-combo-define-local (kbd "=<<") " =<< ")
              (key-combo-define-local (kbd ">>") " >> ")
              (key-combo-define-local (kbd ">>=") " >>= ")

              ;; Arrow
              (key-combo-define-local (kbd "^>>") " ^>> ")
              (key-combo-define-local (kbd ">>^") " >>^ ")
              (key-combo-define-local (kbd ">>>") " >>> ")
              (key-combo-define-local (kbd "<<<") " <<< ")
              (key-combo-define-local (kbd "<<^") " <<^ ")
              (key-combo-define-local (kbd "^<<") " ^<< ")
              (key-combo-define-local (kbd "<+") " <+> ")
              (key-combo-define-local (kbd "**") " ** ")
              (key-combo-define-local (kbd "&&&") " &&& ")
              
              ;; Monoid
              (key-combo-define-local (kbd "<#>") " <#> ")
              
              ;; Sequence
              (key-combo-define-local (kbd "><") " >< ")
              (key-combo-define-local (kbd ":<") " :< ")
              (key-combo-define-local (kbd ":>") " :> ")
              
              ;; GraphRep
              (key-combo-define-local (kbd "|*><*|") " |*><*| ")
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

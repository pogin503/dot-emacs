;;; mylib-keycombo.el --- mylib-keycombo.el
;; This program is free software
;;; Commentary:
;;; Code:

(require 'key-combo)

(defun my-key-combo-global-conf ()
  "My global key-combo setting."
  ;; (key-combo-define-local (kbd "/") "/")
  (key-combo-define-global (kbd "@see") "@see ")
  ;; (key-combo-define-global (kbd "//") "//")
  )

(defun my-key-combo-js-conf ()
  "My key-combo js setting."
  ;; (key-combo-define-local (kbd "/") ("/" "//" "// "))
  (key-combo-define-local (kbd "/*") "/* `!!' */"))

(defun my-c++-mode-key-combo ()
  "My key-combo C++ setting."
  (key-combo-define-local (kbd "//") "// ")
  (key-combo-define-local (kbd "/*") "/* `!!' */")
  (key-combo-define-local (kbd "/**") "/**\n*`!!'\n*/")
  (key-combo-define-local (kbd "::") "::")
  ;; (key-combo-define-local (kbd ">") '(key-combo-execute-orignal " >> "))
  ;; (key-combo-define-local (kbd "<") '(" < " " << "))
  )

(defun my-lisp-mode-key-combo ()
  "My Lisp key-combo setting."
  ;; (key-combo-define-local (kbd ";") '(key-combo-execute-orignal ";=> "))
  (key-combo-define-local (kbd ";=") "; => ")
  (key-combo-define-local (kbd "\\#'") "\\#'()")
  ;; (key-combo-define-local (kbd "\\#'") "\\#'()")
  )

(defun my-key-combo-haskell-conf ()
  "My Haskell key-combo setting."
  (key-combo-define-local (kbd "," ) ", ")
  ;; (key-combo-define-local (kbd "!") 'key-combo-execute-orignal)
  ;; (key-combo-define-local (kbd "$" ) '(" $ " " $! "))

  ;; bind function, Eq
  ;; (key-combo-define-local (kbd "=" ) '(" = " " == "))
  (key-combo-define-local (kbd "=" ) " = ")
  (key-combo-define-local (kbd "==") " == ")
  (key-combo-define-local (kbd "/=") " /= ")

  ;; (key-combo-define-local (kbd "::" ) " :: ")
  ;; ;; Num, List, Arrow
  ;; (key-combo-define-local (kbd "+") '(" + " " ++ " " +++ "))

  ;; ;; Num, comment
  (key-combo-define-local (kbd "-") " - ")
  (key-combo-define-local (kbd "--") "-- ")
  ;; ;; comment
  (key-combo-define-local (kbd "{-") "{- `!!' -}")
  ;; ;; これはまだhaskell-modeが対応していないようなので無理。
  (key-combo-define-local (kbd "{-RET") "{-\n`!!'\n-}")

  (key-combo-define-local (kbd "->") " -> ")
  (key-combo-define-local (kbd "<-") " <- ")
  (key-combo-define-local (kbd "=>") " => ")

  ;; ;; Ord
  ;; (key-combo-define-local (kbd ">") " > ") ;; Ord(>) or Sequence >
  ;; (key-combo-define-local (kbd ">=") " >= ")
  ;; (key-combo-define-local (kbd "<") " < ") ;; Ord (<) or Sequence <
  (key-combo-define-local (kbd "<=") " <= ")

  (key-combo-define-local (kbd "=<<") " =<< ")

  ;; ;; Fractional, Array
  ;; (key-combo-define-local (kbd "/") '(" / " " // "))

  ;; ;; pattern-match, Bool, Arrow
  (key-combo-define-local (kbd "|") " | ")
  (key-combo-define-local (kbd "||") " || ")
  (key-combo-define-local (kbd "|||") " ||| ")

  ;; ;; (key-combo-define-local (kbd "&") '(" & " " && "))
  ;; ;; List
  ;; (key-combo-define-local (kbd "!!") " !! ")

  ;; ;; Applicative
  ;; (key-combo-define-local (kbd "<*>") " <*> ") ;; Applicative (<*>) or GraphRep <*
  ;; (key-combo-define-local (kbd "<*") " <* ")
  ;; (key-combo-define-local (kbd "*>") " *> ")
  ;; (key-combo-define-local (kbd "<**>") " <**> ")

  ;; ;; Alternative
  ;; (key-combo-define-local (kbd "<|") " <|> ")

  ;; ;; Functor
  ;; (key-combo-define-local (kbd "<$") " <$ ")
  ;; (key-combo-define-local (kbd "<$>") " <$> ")

  ;; ;; Monad
  ;; (key-combo-define-local (kbd "=<<") " =<< ")
  ;; (key-combo-define-local (kbd ">>") " >> ")
  (key-combo-define-local (kbd ">>=") " >>= ")

  ;; ;; Arrow
  ;; (key-combo-define-local (kbd "^>>") " ^>> ")
  ;; (key-combo-define-local (kbd ">>^") " >>^ ")
  ;; (key-combo-define-local (kbd ">>>") " >>> ")
  ;; (key-combo-define-local (kbd "<<<") " <<< ")
  ;; (key-combo-define-local (kbd "<<^") " <<^ ")
  ;; (key-combo-define-local (kbd "^<<") " ^<< ")
  ;; (key-combo-define-local (kbd "<+") " <+> ")
  ;; (key-combo-define-local (kbd "**") " ** ")
  ;; (key-combo-define-local (kbd "&&&") " &&& ")
  (key-combo-define-local (kbd "-<") " -< ")
  ;; ;; Monoid
  ;; (key-combo-define-local (kbd "<#>") " <#> ")

  ;; ;; Sequence
  ;; (key-combo-define-local (kbd "><") " >< ")
  ;; (key-combo-define-local (kbd ":<") " :< ")
  ;; (key-combo-define-local (kbd ":>") " :> ")

  ;; ;; GraphRep
  ;; (key-combo-define-local (kbd "|*><*|") " |*><*| ")

  ;; Yesod
  (key-combo-define-local (kbd "{-#") "{-# `!!' #-}")
  )

(defun my-coq-mode-key-combo ()
  "My Coq key-combo setting."
  (key-combo-define-local (kbd ",") ", ")
  (key-combo-define-local (kbd ":") " : ")
  (key-combo-define-local (kbd "->") " -> ")
  (key-combo-define-local (kbd ":=") " := ")
  (key-combo-define-local (kbd "=>") " => ")
  (key-combo-define-local (kbd "(*") "(* `!!' *)")
  (key-combo-define-local (kbd "|") " | ")
  )

(defun my-prolog-mode-key-combo ()
  "My key-combo Prolog setting."
  (key-combo-define-local (kbd ",") ", ")
  (key-combo-define-local (kbd ":-") " :- ")
  (key-combo-define-local (kbd "/*") "/* `!!' */")
  )

(defun my-coffee-mode-key-combo ()
  "Set coffee script key combo."
  (key-combo-define-local (kbd "###") "###\n`!!'###")
  (key-combo-define-local (kbd "=") " = ")
  )

(defun my-ruby-mode-key-combo ()
  "Set Ruby key combo."
  (key-combo-define-local (kbd "::") "::")
  )

(defun my-makefile-key-combo-conf ()
  "My key-combo Makefile setting."
  (key-combo-define-local (kbd "$") '("$" "$(`!!')"))
  (key-combo-define-local (kbd ":") ": "))

(provide 'mylib-keycombo)
;;; mylib-keycombo.el ends here

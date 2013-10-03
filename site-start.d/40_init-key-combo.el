;;; 40_init-key-combo --- 40_init-key-combo
;; This program is free software
;;; Commentary:
;;; Code:
(require 'key-combo)
(key-combo-mode 1)

(key-combo-load-default)

(key-combo-define-global (kbd "@see") "@see ")


(defun my-c++-mode-key-combo ()
  (key-combo-define-local (kbd "//") "// ")
  (key-combo-define-local (kbd "/*") "/* `!!' */")
  (key-combo-define-local (kbd "/**") "/**\n*`!!'\n*/")
  (key-combo-define-local (kbd "::") "::")
  ;; (key-combo-define-local (kbd ">") '(key-combo-execute-orignal " >> "))
  ;; (key-combo-define-local (kbd "<") '(" < " " << "))
  )

(add-hook 'c++-mode-hook 'my-c++-mode-key-combo)

(defun my-lisp-mode-key-combo ()
  ;; (key-combo-define-local (kbd ";") '(key-combo-execute-orignal ";=> "))
  (key-combo-define-local (kbd ";=") "; => ")
  (key-combo-define-local (kbd "\\#'") "\\#'()")
  ;; (key-combo-define-local (kbd "\\#'") "\\#'()")
  )

(add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-key-combo)

(add-hook 'lisp-interaction-mode-hook 'my-lisp-mode-key-combo)

;; (key-combo-define-global (kbd "(") "(`!!')" ")")
;; (key-combo-define-global (kbd "\"") "(`!!')" "\"")
;; (key-combo-define-global KEYS COMMAND &optional GUARD)

;; @see http://d.hatena.ne.jp/marony0607/20111205/1323103005
;; ここの表を参考にいくつかkey-comboを作った。

(defun my-key-combo-haskell-conf ()
  (key-combo-define-local (kbd "," ) ", ")
  ;; (key-combo-define-local (kbd "!") 'key-combo-execute-orignal)
  ;; (key-combo-define-local (kbd "$" ) '(" $ " " $! "))

  ;; bind function, Eq
  ;; (key-combo-define-local (kbd "=" ) '(" = " " == "))
  (key-combo-define-local (kbd "=" ) " = ")
  (key-combo-define-local (kbd "==") " == ")
  (key-combo-define-local (kbd "/=") " /= ")

  (key-combo-define-local (kbd "::" ) " :: ")
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
  (key-combo-define-local (kbd ">=") " >= ")
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
  ;; (key-combo-define-local (kbd ">>=") " >>= ")

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

(add-hook 'haskell-mode-hook 'my-key-combo-haskell-conf)

;; (add-hook 'sh-mode-hook
;;           #'(lambda ()
;;               (key-combo-define-local (kbd "=") "=")
;;               ))

(add-hook 'makefile-mode-hook
          #'(lambda ()
              (key-combo-define-local (kbd "$") '("$" "$(`!!')"))
              (key-combo-define-local (kbd ":") ": ")
              ))

(defun my-coq-mode-key-combo ()
  (key-combo-define-local (kbd ",") ", ")
  (key-combo-define-local (kbd ":") " : ")
  (key-combo-define-local (kbd "->") " -> ")
  (key-combo-define-local (kbd ":=") " := ")
  (key-combo-define-local (kbd "=>") " => ")
  (key-combo-define-local (kbd "(*") "(* `!!' *)")
  (key-combo-define-local (kbd "|") " | ")
  )

;; (add-hook 'coq-mode-hook 'my-coq-mode-key-combo)
;; (add-hook 'proof-activate-scripting-hook 'my-coq-mode-key-combo)
;; (dont-compile
;;   (when (fboundp 'describe)
;;     (describe ("align test in temp-buffer" :vars ((mode)))
;;       ())))

(defun my-prolog-mode-key-combo ()
  (key-combo-define-local (kbd ",") ", ")
  (key-combo-define-local (kbd ":-") " :- ")
  (key-combo-define-local (kbd "/*") "/* `!!' */")
  )

(add-hook 'prolog-mode-hook 'my-prolog-mode-key-combo)

(key-combo-mode 1)

(provide '40_init-key-combo)
;;; 40_init-key-combo ends here

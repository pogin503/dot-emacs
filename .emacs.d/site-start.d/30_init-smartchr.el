;;@see https://github.com/handle/emacs-settings
;; INSTALL
;; (install-elisp "https://github.com/imakado/emacs-smartchr/raw/master/smartchr.el")

(req smartchr)

;; (defun my-smartchr-setting ()
;;   (local-set-key (kbd "=") (smartchr '("=" " = " " == ")))
;;   (local-set-key (kbd "+") (smartchr '("+" " + " "++" " += ")))
;;   (local-set-key (kbd "-") (smartchr '("-" " - " "--" " -= ")))

;;   (local-set-key (kbd "\"") (smartchr '("\"" "\"`!!'\"")))
;;   (local-set-key (kbd "'") (smartchr '("'" "'`!!''")))

;;   (local-set-key (kbd ">") (smartchr '(">" "->" ">>")))

;;   (local-set-key (kbd "(") (smartchr '("(" "(`!!')")))
;;   (local-set-key (kbd "{") (smartchr '("{" "{ `!!' }" "{\n`!!'\n}")))
;;   (local-set-key (kbd "[") (smartchr '("[" "[`!!']")))
;;   )

(defun my-smartchr-keybindings ()
  (local-set-key (kbd "=") (smartchr '("=" " = " " == ")))
  ;; !! がカーソルの位置
  (local-set-key (kbd "+") (smartchr '("+" " + " "++" " += ")))
  (local-set-key (kbd "-") (smartchr '("-" " - " "--" " -= ")))
  (local-set-key (kbd "(") (smartchr '("(`!!')" "(")))
  (local-set-key (kbd "[") (smartchr '("[`!!']" "[[`!!']]" "[")))
  (local-set-key (kbd "{") (smartchr '("{\n`!!'\n}" "{`!!'}" "{")))
  (local-set-key (kbd "`") (smartchr '("\``!!''" "\`")))
  (local-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\"")))
  (local-set-key (kbd ">") (smartchr '(" > " "->" " >> " " => " ">")))
  (local-set-key (kbd "<") (smartchr '("<" " < " " << " "<`!!'>")))
  (local-set-key (kbd ",") (smartchr '(", " ",")))
  (local-set-key (kbd ".") (smartchr '("." " . ")))
  (local-set-key (kbd "?") (smartchr '("?" "? `!!' " "<?`!!'?>")))
  (local-set-key (kbd "!") (smartchr '("!" " != ")))
  (local-set-key (kbd "&") (smartchr '("&" " && ")))
  (local-set-key (kbd "|") (smartchr '("|" " || ")))
  )
(defun my-smartchr-keybindings-lisp ()
  (local-set-key (kbd "(") (smartchr '("(`!!')" "(")))
  (local-set-key (kbd "[") (smartchr '("[`!!']" "[")))
  (local-set-key (kbd "`") (smartchr '("\``!!''" "\`")))
  (local-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\"")))
  (local-set-key (kbd ".") (smartchr '("." " . ")))
  )  

(defun my-smartchr-keybindings-objc ()
  (local-set-key (kbd "@") (smartchr '("@\"`!!'\"" "@"))))

(add-hook 'c-mode-common-hook 'my-smartchr-keybindings)
(add-hook 'c++-mode-hook 'my-smartchr-keybindings)
(add-hook 'objc-mode-hook 'my-smartchr-keybindings-objc)

(add-hook 'php-mode-hook 'my-smartchr-keybindings)

(add-hook 'ruby-mode-hook 'my-smartchr-keybindings)

(add-hook 'cperl-mode-hook 'my-smartchr-keybindings)

(add-hook 'emacs-lisp-mode-hook 'my-smartchr-keybindings-lisp)
(add-hook 'inferior-gauche-mode 'my-smartchr-keybindings-lisp)
(add-hook 'scheme-mode 'my-smartchr-keybindings-lisp)

(add-hook 'javascript-mode-hook 'my-smartchr-keybindings)
(add-hook 'js-mode-hook 'my-smartchr-keybindings)
(add-hook 'js2-mode-hook 'my-smartchr-keybindings)
;;(remove-hook 'javascript-mode-hook 'my-smartchr-keybindings)

(defun my-smartchr-keybindings-html ()
   (local-set-key (kbd "=") (smartchr '("=" " = " " == ")))
  ;; !! がカーソルの位置
  (local-set-key (kbd "(") (smartchr '("(`!!')" "(")))
  (local-set-key (kbd "[") (smartchr '("[`!!']" "[[`!!']]" "[")))
  (local-set-key (kbd "{") (smartchr '("{\n`!!'\n}" "{`!!'}" "{")))
  (local-set-key (kbd "`") (smartchr '("\``!!''" "\`")))
  (local-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\"")))
  (local-set-key (kbd ">") (smartchr '(" > " "->" " >> " " => ")))
  (local-set-key (kbd "<") (smartchr '("<`!!'>" "</`!!'>" "<")))
  (local-set-key (kbd ",") (smartchr '(", " ",")))
  (local-set-key (kbd ".") (smartchr '("." " . ")))
  (local-set-key (kbd "?") (smartchr '("?" "? `!!' " "<?`!!'?>")))
  (local-set-key (kbd "!") (smartchr '("!" " != ")))
  (local-set-key (kbd "&") (smartchr '("&" " && ")))
  (local-set-key (kbd "|") (smartchr '("|" " || ")))
  )

(add-hook 'html-mode-hook 'my-smartchr-keybindings-html)

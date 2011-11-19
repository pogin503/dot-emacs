;;@see https://github.com/handle/emacs-settings
;;@see https://bitbucket.org/sakito/dot.emacs.d/src/7a6d89a5b3c6/site-start.d/init_smartchr.el
;; INSTALL
;; (install-elisp "https://github.com/imakado/emacs-smartchr/raw/master/smartchr.el")

(req smartchr)

(defun my-smartchr-keybindings ()
  ;; !! がカーソルの位置
  (local-set-key (kbd "=") (smartchr '("=" " = " " == ")))
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
  (local-set-key (kbd "[") (smartchr '("[`!!']" "[[`!!']]" "[")))
  (local-set-key (kbd "`") (smartchr '("\``!!''" "\`")))
  (local-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\"")))
  (local-set-key (kbd ".") (smartchr '("." " . ")))
  )

(defun my-smartchr-keybindings-objc ()
  (local-set-key (kbd "@") (smartchr '("@\"`!!'\"" "@"))))

(dolist (hook (list
               'c-mode-common-hook    
               'c++-mode-hook              
               'php-mode-hook              
               'ruby-mode-hook             
               'cperl-mode-hook            
               'javascript-mode-hook       
               'js-mode-hook               
               'js2-mode-hook
               ))
  (add-hook hook 'my-smartchr-keybindings))

(dolist (hook (list
               'emacs-lisp-mode-hook       
               'lisp-interaction-mode-hook 
               'inferior-gauche-mode       
               'scheme-mode                
               ))
  (add-hook hook 'my-smartchr-keybindings))


(add-hook 'objc-mode-hook             'my-smartchr-keybindings-objc)

(defun my-smartchr-keybindings-html ()
  ;; !! がカーソルの位置
  (local-set-key (kbd "=") (smartchr '("=" " = " " == ")))
  (local-set-key (kbd "(") (smartchr '("(`!!')" "(")))
  (local-set-key (kbd "[") (smartchr '("[`!!']" "[[`!!']]" "[")))
  (local-set-key (kbd "{") (smartchr '("{\n`!!'\n}" "{`!!'}" "{")))
  (local-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\"")))
  (local-set-key (kbd "<") (smartchr '("<`!!'>" "</`!!'>" "<")))
  (local-set-key (kbd ",") (smartchr '(", " ",")))
  (local-set-key (kbd ".") (smartchr '("." " . ")))
  (local-set-key (kbd "?") (smartchr '("?" "? `!!' " "<?`!!'?>")))
  (local-set-key (kbd "!") (smartchr '("!" " != ")))
  (local-set-key (kbd "&") (smartchr '("&" " && ")))
  (local-set-key (kbd "|") (smartchr '("|" " || ")))
  )

(add-hook 'html-mode-hook 'my-smartchr-keybindings-html)


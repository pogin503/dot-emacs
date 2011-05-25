;;@see https://github.com/handle/emacs-settings
;; INSTALL
;; (install-elisp "https://github.com/imakado/emacs-smartchr/raw/master/smartchr.el")

(require 'smartchr)

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

(defun smartchr-custom-keybindings ()
  (local-set-key (kbd "=") (smartchr '(" = " " == "  "=")))
  ;; !! がカーソルの位置
  (local-set-key (kbd "+") (smartchr '("+" " + " "++" " += ")))
  (local-set-key (kbd "-") (smartchr '("-" " - " "--" " -= ")))
  (local-set-key (kbd "(") (smartchr '("(`!!')" "(")))
  (local-set-key (kbd "[") (smartchr '("[`!!']" "[ [`!!'] ]" "[")))
  (local-set-key (kbd "{") (smartchr '("{\n`!!'\n}" "{`!!'}" "{")))
  (local-set-key (kbd "`") (smartchr '("\``!!''" "\`")))
  (local-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\"")))
  (local-set-key (kbd ">") (smartchr '(">" " => " " => '`!!''" " => \"`!!'\"")))
  (local-set-key (kbd "<") (smartchr '("<" "<`!!'>")))
  )

(defun my-smartchr-lisp-keybindings ()
  (local-set-key (kbd "(") (smartchr '("(`!!')" "(")))
  (local-set-key (kbd "[") (smartchr '("[`!!']" "[ [`!!'] ]" "[")))
  (local-set-key (kbd "`") (smartchr '("\``!!''" "\`")))
  (local-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\"")))
  )  

(defun smartchr-custom-keybindings-objc ()
  (local-set-key (kbd "@") (smartchr '("@\"`!!'\"" "@"))))

(add-hook 'c-mode-common-hook 'smartchr-custom-keybindings)
(add-hook 'c++-mode-hook 'smartchr-custom-keybindings)
(add-hook 'objc-mode-hook 'smartchr-custom-keybindings-objc)
(add-hook 'php-mode-hook 'my-smartchr-setting)
(add-hook 'javascript-mode-hook 'my-smartchr-setting)
(add-hook 'ruby-mode-hook 'my-smartchr-setting)
(add-hook 'cperl-mode-hook 'my-smartchr-setting)
(add-hook 'emacs-lisp-mode-hook 'my-smartchr-lisp-keybindings)





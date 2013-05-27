(require 'git-gutter)
(global-git-gutter-mode t)

(setq git-gutter:separator-sign "|")
(set-face-foreground 'git-gutter:separator "yellow")
(setq git-gutter:modified-sign "  ") ;; two space
(setq git-gutter:added-sign "++")    ;; multiple character is OK
(setq git-gutter:deleted-sign "--")

(set-face-background 'git-gutter:modified "purple") ;; background color
(set-face-foreground 'git-gutter:added "green")
(set-face-foreground 'git-gutter:deleted "red")

(add-hook 'cperl-mode-hook 'git-gutter-mode)
(add-hook 'js2-mode-hook   'git-gutter-mode)
(add-hook 'php-mode-hook   'git-gutter-mode)
(add-hook 'ruby-mode-hook  'git-gutter-mode)
(add-hook 'emacs-lisp-mode-hook  'git-gutter-mode)

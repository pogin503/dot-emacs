;;; 00_init-package.el --- package conf
;;; Commentary:
;;; Code:
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)

(package-initialize)
(require 'cask)
(let ((bundle (cask-initialize)))
  (cask-install bundle)
  (add-to-list 'auto-mode-alist '("Cask$" . emacs-lisp-mode)))

(require 'use-package)

(use-package pallet
  :config
  (pallet-mode +1))

(use-package ag
  :ensure ag)

(use-package elisp-slime-nav
  :ensure elisp-slime-nav
  :config
  (progn
	(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
	  (add-hook hook 'turn-on-elisp-slime-nav-mode))))

(use-package org
  :commands (org-mode org-todo-list org-agenda org-store-link)
  :bind (("C-c a" . org-agenda)
		 ("C-c c" . org-capture)
		 ("C-c l" . org-store-link)
		 ("C-c b" . org-iswitchb))
  :config
  ;; (define-key org-mode-map (kbd "C-m") 'org-return-indent)
  (use-package org-export-generic)
  )


(use-package ido
  :config
  (progn
	(ido-mode +1)
	(custom-set-variables
	 '(ido-enable-flex-matching t)
	 '(ido-use-filename-at-point 'guess)
	 '(ido-everywhere t)
	 '(ido-use-faces nil)
	 '(ido-ignore-extensions t))
	))

(use-package multiple-cursors
  :bind (("C-c C-S-c" . mc/edit-lines)
		 ("C->" . mc/mark-next-like-this)
		 ("C-<" . mc/mark-previous-like-this)
		 ("C-c C-<"  . mc/mark-all-like-this)
		 ("C-*"  . mc/mark-all-like-this)))

(use-package volatile-highlights
  :config
  (volatile-highlights-mode t))

(use-package volatile-highlights
  :config
  (volatile-highlights-mode t))

(use-package buffer-move
  :config
  (progn
	(global-set-key (kbd "C-c <left>")  'buf-move-left)
	(global-set-key (kbd "C-c <down>")  'buf-move-down)
	(global-set-key (kbd "C-c <up>")    'buf-move-up)
	(global-set-key (kbd "C-c <right>") 'buf-move-right)
	))

(use-package ats-mode
  :mode ("\\.dats\\'" . ats-mode)
  :commands (ats-mode))

;;; 00_init-package.el ends here

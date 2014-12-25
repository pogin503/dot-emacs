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
  (progn
	(if (boundp 'dropbox-directory)
		(setq org-directory (concat dropbox-directory "/org/"))
	  (setq org-directory "~/Documents/org/"))

	(setq org-default-notes-file (concat org-directory "agenda.org"))
	(setq org-agenda-files
		  (mapcar #'(lambda (x) (concat org-directory x))
				  '("work.org" "home.org" "agendasample.org" "test.org")))

	;; MobileOrg
	;; (setq org-mobile-directory (concat org-directory "MobileOrg"))
	;; (setq org-mobile-inbox-for-pull (concat org-directory "flagged.org"))

	(setq org-startup-truncated nil)	   ;; org-modeでの折り返し設定
	(setq org-return-follows-link t)	   ;; リンクをreturnで追う

	(setq org-log-done 'time) ;; DONEの時刻を記録

	;; TODO状態
	(setq org-todo-keywords
		  '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "SOMEDAY(s)")))

	;; (setq org-remember-templates
	;;       '(("Todo" ?t "** TODO %?\n   %i\n   %a\n   %t" nil "Inbox")
	;;         ("Bug" ?b "** TODO %?   :bug:\n   %i\n   %a\n   %t" nil "Inbox")
	;;         ("Idea" ?i "** %?\n   %i\n   %a\n   %t" nil "New Ideas")
	;;         ))

	(setq org-capture-templates
		  '(("t" "Todo" entry (file+headline (concat org-directory "todo.org") "Tasks")
			 "* TODO %?n %in %a")
			("j" "Journal" entry (file+datetree (concat org-directory "journal.org"))
			 "* %?n %Un %in %a")
			("n" "Note" entry (file+headline (cocat org-directory "notes.org") "Notes")
			 "* %?n %Un %i")
			))

	;; (defvar org-code-reading-software-name nil)
	;; ;; ~/memo/code-reading.org に記録する
	;; (defvar org-code-reading-file "code-reading.org")
	;; (defun org-code-reading-read-software-name ()
	;;   (set (make-local-variable 'org-code-reading-software-name)
	;;        (read-string "Code Reading Software: "
	;;                     (or org-code-reading-software-name
	;;                         (file-name-nondirectory
	;;                          (buffer-file-name))))))

	;; (defun org-code-reading-get-prefix (lang)
	;;   (concat "[" lang "]"
	;;           "[" (org-code-reading-read-software-name) "]"))
	;; (defun org-remember-code-reading ()
	;;   (interactive)
	;;   (let* ((prefix (org-code-reading-get-prefix (substring (symbol-name major-mode) 0 -5)))
	;;          (org-remember-templates
	;;           `(("CodeReading" ?r "** %(identity prefix)%?\n   \n   %a\n   %t"
	;;              ,org-code-reading-file "Memo"))))
	;;     (org-remember)))

	(defun my-org-next-visible-link ()
	  "Move forward to the next link.
If the link is in hidden text, expose it."
	  (interactive)
	  (when (and org-link-search-failed (eq this-command last-command))
		(goto-char (point-min))
		(message "Link search wrapped back to beginning of buffer"))
	  (setq org-link-search-failed nil)
	  (let* ((pos (point))
			 (ct (org-context))
			 (a (assoc :link ct))
			 srch)
		(if a (goto-char (nth 2 a)))
		(while (and (setq srch (re-search-forward org-any-link-re nil t))
					(goto-char (match-beginning 0))
					(prog1 (not (eq (org-invisible-p) 'org-link))
					  (goto-char (match-end 0)))))
		(if srch
			(goto-char (match-beginning 0))
		  (goto-char pos)
		  (setq org-link-search-failed t)
		  (error "No further link found"))))

	(defun my-org-previous-visible-link ()
	  "Move backward to the previous link.
If the link is in hidden text, expose it."
	  (interactive)
	  (when (and org-link-search-failed (eq this-command last-command))
		(goto-char (point-max))
		(message "Link search wrapped back to end of buffer"))
	  (setq org-link-search-failed nil)
	  (let* ((pos (point))
			 (ct (org-context))
			 (a (assoc :link ct))
			 srch)
		(if a (goto-char (nth 1 a)))
		(while (and (setq srch (re-search-backward org-any-link-re nil t))
					(goto-char (match-beginning 0))
					(not (eq (org-invisible-p) 'org-link))))
		(if srch
			(goto-char (match-beginning 0))
		  (goto-char pos)
		  (setq org-link-search-failed t)
		  (error "No further link found"))))
	(define-key org-mode-map "\M-n" 'my-org-next-visible-link)
	(define-key org-mode-map "\M-p" 'my-org-previous-visible-link)

	(bind-keys :map org-mode-map
			   ("C-<up>"     outline-previous-visible-heading)
			   ("C-<down>"   outline-next-visible-heading)
			   ("C-S-<up>"   outline-backward-same-level)
			   ("C-S-<down>" outline-forward-same-level)
			   )
	;; (define-key org-mode-map (kbd "C-m") 'org-return-indent)
	(use-package org-export-generic)
	))


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

(use-package popwin
  :init
  (progn
	;;dired
	(push '(dired-mode :position top) popwin:special-display-config)

	;; Apropos
	(push '("*slime-apropos*") popwin:special-display-config)
	;; Macroexpand
	(push '("*slime-macroexpansion*") popwin:special-display-config)
	;; Help
	(push '("*slime-description*") popwin:special-display-config)
	;; Compilation
	(push '("*slime-compilation*" :noselect t) popwin:special-display-config)
	;; Cross-reference
	(push '("*slime-xref*") popwin:special-display-config)
	;; Debugger
	(push '(sldb-mode :stick t) popwin:special-display-config)
	;; REPL
	(push '(slime-repl-mode) popwin:special-display-config)
	;; Connections
	(push '(slime-connection-list-mode) popwin:special-display-config)

	;;@see http://d.hatena.ne.jp/sokutou-metsu/20110205/1296915272
	(push '(" *auto-async-byte-compile*" :height 14 :position bottom :noselect t) popwin:special-display-config)
	(push '("*VC-log*" :height 10 :position bottom) popwin:special-display-config)
	;;Compile-Log
	(push '("*Compile-Log*" :height 10 :noselect t) popwin:special-display-config)
	(push '("*Process List*" :stick t) popwin:special-display-config)
	(push '("*sdic*" :noselect t)  popwin:special-display-config)
	(push '("*init log*" :stick t) popwin:special-display-config)
	(push '("\\*magit.*" :stick t :regexp t :height 25) popwin:special-display-config)
	(push '("COMMIT-EDITMSG" :height 15) popwin:special-display-config)
	(push '("*compilation*" :regexp t) popwin:special-display-config)
	(push '("*ert*" :regexp t) popwin:special-display-config)
	(push '("*Codic Result*" :height 15) popwin:special-display-config))
  :config
  (progn
	(setq popwin:close-popup-window-timer-interval 0.7)
	(setq display-buffer-function 'popwin:display-buffer)
	(popwin-mode 1)
	))

(use-package volatile-highlights
  :config
  (volatile-highlights-mode t))

;; エコーエリアに関数の型を表示するモードをオンにする
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;; haskell-indentationモードを有効にする
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent))
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(use-package haskell-mode
  :commands (haskell-mode literate-haskell-mode haskell-cabal-mode)
  :interpreter (("runghc" . haskell-mode)
				("runhaskell" . haskell-mode))
  :mode (("\\.l?hs$" . haskell-mode)
		 ("\\.cabal$" . haskell-cabal-mode)
		 )
  :config
  (use-package inf-haskell)
  (use-package ghc
	:config
	(progn
	  ;; ghc-mod setting
	  (autoload 'ghc-init "ghc" nil t)
	  ;; (autoload 'ghc-debug "ghc" nil t)
	  ))
  )

(use-package volatile-highlights
  :config
  (volatile-highlights-mode t))

(use-package buffer-move
  :config
  (progn
	(global-set-key (kbd "C-c <left>")  'windmove-left)
	(global-set-key (kbd "C-c <down>")  'windmove-down)
	(global-set-key (kbd "C-c <up>")    'windmove-up)
	(global-set-key (kbd "C-c <right>") 'windmove-right)
	))

(use-package ats-mode
  :mode ("\\.dats\\'" . ats-mode)
  :commands (ats-mode))

;; (use-package helm-apropos)

;; (require 'mylib)
;; (my-install-package installing-package-list)

;;; 00_init-package.el ends here

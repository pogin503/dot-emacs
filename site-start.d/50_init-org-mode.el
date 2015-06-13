;;; 50_init-org-mode --- 50_init-org-mode
;; This program is free software
;;; Commentary:
;;@see http://d.hatena.ne.jp/rubikitch/20090121/1232468026
;;; Code:

(use-package org
  :commands (org-mode org-todo-list org-agenda org-store-link)
  :bind (
         ;; org-agendaはアジェンダ(行動計画)を立てるための機能
         ("C-c a" . org-agenda)
         ;; org-modeでのメモをする機能
		 ("C-c c" . org-capture)
         ;; orgモードを使って現在のファイル行へのリンクを保存する
		 ("C-c l" . org-store-link)
         ;; orgバッファのみを行き来するiswitchbコマンド
		 ("C-c b" . org-iswitchb))
  :config
  (if (boundp 'dropbox-directory)
      (setq org-directory (concat dropbox-directory "Documents/"))
    (setq org-directory "~/Documents/org/"))

  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline (concat org-directory "todo.org") "Tasks")
           "* TODO %?n %in %a")
          ("j" "Journal" entry (file+datetree (concat org-directory "journal.org"))
           "* %?n %Un %in %a")
          ("n" "Note" entry (file+headline (cocat org-directory "notes.org") "Notes")
           "* %?n %Un %i")
          ))

  (setq org-agenda-files
        (list
         (concat org-directory "work.org")
         (concat org-directory "home.org")
         org-directory))

  ;; fontify code in code blocks
  (setq org-src-fontify-natively t)
  )

;; | C-c l | 現在のファイル行のリンクを保存する |
;; | C-c a | org-modeでメモをする |
;; | C-c c | アジェンダ(行動計画)機能を起動する |
;; | C-c b | orgバッファのみ行き来する |

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook '(lambda () (org-bullets-mode 1)))
  )

(provide '50_init-org-mode)
;;; 50_init-org-mode ends here

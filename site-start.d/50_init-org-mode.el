;;; 50_init-org-mode --- 50_init-org-mode
;; This program is free software
;;; Commentary:
;;@see http://d.hatena.ne.jp/rubikitch/20090121/1232468026
;;; Code:

(require '00_init-macro)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; orgモードを使って現在のファイル行へのリンクを保存する
(global-set-key "\C-c l" 'org-store-link)

;; org-modeでのメモをする機能
(global-set-key "\C-c c" 'org-capture)

;; org-agendaの起動
;; org-agendaはアジェンダ(行動計画)を立てるための機能
(global-set-key "\C-c a" 'org-agenda)

;; orgバッファのみを行き来するiswitchbコマンド
(global-set-key "\C-c b" 'org-iswitchb)

;; | C-c l | 現在のファイル行のリンクを保存する |
;; | C-c a | org-modeでメモをする |
;; | C-c c | アジェンダ(行動計画)機能を起動する |
;; | C-c b | orgバッファのみ行き来する |

(lazyload (org-mode org-todo-list org-agenda org-store-link) "org"
          (require 'org)
          (require 'org-install)

          (if (boundp 'dropbox-directory)
              (setq org-directory (concat dropbox-directory "/Documents/org/"))
            (setq org-directory "~/Documents/org/"))

          (setq org-default-notes-file (concat org-directory "agenda.org"))
          (setq org-agenda-files
                (mapcar #'(lambda (x) (concat org-directory x))
                        '("work.org" "school.org" "home.org")))

          ;; MobileOrg
          (setq org-mobile-directory (concat org-directory "MobileOrg"))
          (setq org-mobile-inbox-for-pull (concat org-directory "flagged.org"))

          (setq org-startup-truncated nil) ;; org-modeでの折り返し設定
          (setq org-return-follows-link t) ;; リンクをreturnで追う

          (setq org-log-done 'time) ;; DONEの時刻を記録

          ;; (org-remember-insinuate)

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

          (define-key org-mode-map [(control up)] 'outline-previous-visible-heading)
          (define-key org-mode-map [(control down)] 'outline-next-visible-heading)
          (define-key org-mode-map [(control shift up)] 'outline-backward-same-level)
          (define-key org-mode-map [(control shift down)] 'outline-forward-same-level)

          ;; (define-key org-mode-map (kbd "C-m") 'org-return-indent)
          (req org-export-generic)
          )

(eval-after-load "org-mode"
  '(progn
    (req org-tree-slide
         (global-set-key (kbd "<f8>") 'org-tree-slide-mode)
         (global-set-key (kbd "S-<f8>") 'org-tree-slide-skip-done-toggle))
    ))

(req org-bullets
     (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
     )

(add-hook 'org-mode-hook #'(lambda () (auto-fill-mode -1)))

(provide '50_init-org-mode)
;;; 50_init-org-mode ends here

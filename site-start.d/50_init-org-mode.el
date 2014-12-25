;;; 50_init-org-mode --- 50_init-org-mode
;; This program is free software
;;; Commentary:
;;@see http://d.hatena.ne.jp/rubikitch/20090121/1232468026
;;; Code:

(require '00_init-macro)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; orgモードを使って現在のファイル行へのリンクを保存する
(global-set-key "\C-cl" 'org-store-link)

;; org-modeでのメモをする機能
(global-set-key "\C-cc" 'org-capture)

;; org-agendaの起動
;; org-agendaはアジェンダ(行動計画)を立てるための機能
(global-set-key "\C-ca" 'org-agenda)

;; orgバッファのみを行き来するiswitchbコマンド
(global-set-key "\C-cb" 'org-iswitchb)

;; | C-c l | 現在のファイル行のリンクを保存する |
;; | C-c a | org-modeでメモをする |
;; | C-c c | アジェンダ(行動計画)機能を起動する |
;; | C-c b | orgバッファのみ行き来する |

(eval-after-load "org"
  '(progn
    (req org-tree-slide
         (global-set-key (kbd "<f8>") 'org-tree-slide-mode)
         (global-set-key (kbd "S-<f8>") 'org-tree-slide-skip-done-toggle))
    ))

(req org-bullets
     (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
     )

(add-hook 'org-mode-hook (lambda () (auto-fill-mode -1)))

(provide '50_init-org-mode)
;;; 50_init-org-mode ends here

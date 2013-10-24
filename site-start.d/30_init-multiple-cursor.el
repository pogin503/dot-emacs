;;; 30_init-multiple-cursor --- 30_init-multiple-cursor
;; This program is free software
;;; Commentary:
;;; Code:
(require 'multiple-cursors)

(global-set-key (kbd "C-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-*") 'mc/mark-all-like-this)
(provide '30_init-multiple-cursor)
;;; 30_init-multiple-cursor ends here

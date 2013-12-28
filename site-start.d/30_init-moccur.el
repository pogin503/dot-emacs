;;; 30_init-moccur --- 30_init-moccur
;; This program is free software
;;; Commentary:
;;; Code:
(require '00_init-macro)

(req color-moccur)

(eval-after-load "color-moccur"
  '(req moccur-edit))

(defadvice moccur-edit-change-file
  (after save-after-moccur-edit-buffer activate)
  (save-buffer))

(provide '30_init-moccur)
;;; 30_init-moccur ends here

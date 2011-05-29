(req color-moccur)

(eval-after-load "color-moccur"
  '(req moccur-edit))

(defadvice moccur-edit-change-file
 (after save-after-moccur-edit-buffer activate)
 (save-buffer))


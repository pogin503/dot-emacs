(require 'color-moccur)

(eval-after-load "color-moccur"
  '(require 'moccur-edit))

(defadvice moccur-edit-change-file
 (after save-after-moccur-edit-buffer activate)
 (save-buffer))


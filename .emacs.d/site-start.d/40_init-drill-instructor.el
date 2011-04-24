;鬼軍曹.el
;(require 'drill-instructor)
;(setq drill-instructor-global nil)

;; ↓ここから追加
;真鬼軍曹
;(mapc (lambda (name)
;        (fset name 'kill-emacs))
;      '(drill-instructor-alert-up
;        drill-instructor-alert-down
;        drill-instructor-alert-right
;        drill-instructor-alert-left
;        drill-instructor-alert-del
;        drill-instructor-alert-return
;        drill-instructor-alert-tab))
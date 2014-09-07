;;; 30_init-ido --- 30_init-ido
;; This program is free software
;;; Commentary:
;;; Code:

(require 'ido)

(custom-set-variables
 '(ido-enable-flex-matching t)
 '(ido-use-filename-at-point 'guess)
 '(ido-everywhere t)
 '(ido-use-faces nil)
 '(ido-ignore-extensions t))

(ido-mode 1)

(provide '30_init-ido)
;;; 30_init-ido ends here

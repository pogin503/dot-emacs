;;; cocoa-emacs-00-setting.el --- MacOSX setting
;;; Commentary:
;; This program is free software
;;; Code:

;; encoding
(require 'ucs-normalize)
(prefer-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8-hfs)
(setq locale-coding-system 'utf-8-hfs)

(defun ucs-normalize-NFC-buffer ()
  "バッファ中の"
  (interactive)
  (ucs-normalize-NFC-region (point-min) (point-max))
  )

(global-set-key (kbd "C-x RET u") 'ucs-normalize-NFC-buffer)

(provide 'cocoa-emacs-00-setting)
;;; cocoa-emacs-00-setting.el ends here

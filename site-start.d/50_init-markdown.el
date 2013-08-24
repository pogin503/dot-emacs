;;; 50_init-markdown --- markdown conf
;;; Commentary:
;; for markdown-mode conf
;;; Code:
(require 'markdown-mode)

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(provide '50_init-markdown)
;;; 50_init-markdown ends here

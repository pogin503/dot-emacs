;;; 50_init-clojure --- 50_init-clojure
;; This program is free software
;;; Commentary:
;;; Code:

(add-hook 'clojure-mode-hook 'cider-mode)

;; RELPのbuffer名を 'project名:nREPLのport番号' と表示する
;; project名は project.clj で defproject した名前
(setq nrepl-buffer-name-show-port t)

(autoload 'ac-nrepl "ac-nrepl" nil t)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))

(provide '50_init-clojure)
;;; 50_init-clojure ends here

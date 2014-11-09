;;; 50_init-clojure --- 50_init-clojure
;; This program is free software
;;; Commentary:
;;; Code:

(require '00_init-macro)

(when (autoload-if-found 'clojure-mode "clojure-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
  (lazyload (clojure-mode) "clojure-mode"
            (require 'clojure-mode)))

(add-hook 'clojure-mode-hook 'cider-mode)

;; mini bufferに関数の引数を表示させる
;; (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; 'C-x b' した時に *nrepl-connection* と *nrepl-server* のbufferを一覧に表示しない
(setq nrepl-hide-special-buffers t)

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

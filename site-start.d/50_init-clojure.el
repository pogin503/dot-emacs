;;; 50_init-clojure --- 50_init-clojure
;; This program is free software
;;; Commentary:
;;; Code:

(require '00_init-macro)

(when (autoload-if-found 'clojure-mode "clojure-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
  (lazyload (clojure-mode) "clojure-mode"
            (require 'clojure-mode)))

(provide '50_init-clojure)
;;; 50_init-clojure ends here

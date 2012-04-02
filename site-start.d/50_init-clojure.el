(when (autoload-if-found 'clojure-mode "clojure-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
  (lazyload (clojure-mode) "clojure-mode"
            (require 'clojure-mode)))

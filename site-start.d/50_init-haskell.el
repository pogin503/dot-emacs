(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(lazyload (haskell-mode) "haskell-mode"
          (require 'haskell-mode))

;; (eval-after-load "~/plugins/haskell-mode/haskell-mode.el"
;;   )
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent))

(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs$" . literate-haskell-mode))
;; (add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))

(add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))     ;; #!/usr/bin/env runghc 用
(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode)) ;;#!/usr/bin/env runhaskell 用


(lazyload (haskell-mode) "haskell-mode"
          (require 'haskell-mode))

;; (lazyload (haskell-cabal-mode) "haskell-cabal-mode"
;;           (require 'haskell-cabal-mode))

;; (eval-after-load "~/plugins/haskell-mode/haskell-mode.el"
;;   )
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent))

;;; auto-complete
(add-hook 'haskell-mode-hook 'auto-complete-mode)

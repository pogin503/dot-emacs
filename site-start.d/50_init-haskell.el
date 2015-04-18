;;; 50_init-haskell --- 50_init-haskell
;; This program is free software
;;; Commentary:
;;; Code:

;; #!/usr/bin/env runghc 用
(add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))
;; #!/usr/bin/env runhaskell 用
(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode))

(add-to-list 'auto-mode-alist '("\\.hamlet$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.lucius$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.julius$" . js2-mode))

(require 'mylib)

(require '00_init-macro)
(lazyload (haskell-mode literate-haskell-mode haskell-cabal-mode) "haskell-mode"
          (require 'haskell-mode)
          (require 'haskell-cabal)
          (require 'inf-haskell)

          (defun my-ac-haskell-mode ()
            (setq ac-sources '(ac-source-words-in-same-mode-buffers ac-source-dictionary ac-source-ghc-mod)))
          )

;; エコーエリアに関数の型を表示するモードをオンにする
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent))
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(use-package haskell-mode
  :commands (haskell-mode literate-haskell-mode haskell-cabal-mode)
  :interpreter (("runghc" . haskell-mode)
				("runhaskell" . haskell-mode))
  :mode (("\\.l?hs$" . haskell-mode)
		 ("\\.cabal$" . haskell-cabal-mode)
		 )
  :config
  (use-package ghc
	:config
	(progn
	  ;; ghc-mod setting
	  (autoload 'ghc-init "ghc" nil t)
	  ;; (autoload 'ghc-debug "ghc" nil t)
	  ))
  )


;; エコーエリアに関数の型を表示するモードをオンにする
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

;; haskell-indentationモードを有効にする
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent))
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; path setting
(when (or run-linux run-darwin)
  (add-to-list 'exec-path (expand-file-name "~/.cabal/bin"))
  (when run-darwin
    (add-to-list 'exec-path "~/Library/Haskell/bin")))

(if run-windows
    (add-to-list 'exec-path
                 (concat "C:/Users/" user-login-name "/AppData/Roaming/cabal/bin")))


(defun my-haskell-mode-conf ()
  "Set haskell-mode config."
  (ghc-init)
  (my-ac-haskell-mode)
  (interactive-haskell-mode))

(add-hook 'haskell-mode-hook 'my-haskell-mode-keybinds)
(add-hook 'haskell-mode-hook 'my-haskell-mode-conf)
(add-hook 'haskell-mode-hook 'my-key-combo-haskell-conf)

;; Haskell align setting
(add-to-list 'align-rules-list
             '(haskell-types
               (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))
(add-to-list 'align-rules-list
             '(haskell-assignment
               (regexp . "\\(\\s-+\\)=\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))
(add-to-list 'align-rules-list
             '(haskell-arrows
               (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))
(add-to-list 'align-rules-list
             '(haskell-left-arrows
               (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))

;; @see http://karky7.blogspot.jp/2012/12/gentooemacshaskell.html
;; (defadvice inferior-haskell-load-file (after change-focus-after-load)
;;   "Change focus to GHCi window after \\<haskell-mode-map>\\[inferior-haskell-load-file] command."
;;   (other-window 1))
;; (ad-activate 'inferior-haskell-load-file)

(provide '50_init-haskell)
;;; 50_init-haskell ends here

;;; 50_init-haskell --- 50_init-haskell
;; This program is free software
;;; Commentary:
;;; Code:

(require 'use-package)

;; path setting
(when (or run-linux run-darwin)
  (add-to-list 'exec-path (expand-file-name "~/.cabal/bin"))
  (when run-darwin
    (add-to-list 'exec-path "~/Library/Haskell/bin")))

(if run-windows
    (add-to-list 'exec-path
                 (concat "C:/Users/" user-login-name "/AppData/Roaming/cabal/bin")))

(use-package haskell-mode
  :commands (haskell-mode literate-haskell-mode haskell-cabal-mode)
  ;; :interpreter (("runghc" . haskell-mode) ; #!/usr/bin/env runghc 用
	;; 			("runhaskell" . haskell-mode)) ; #!/usr/bin/env runhaskell 用
  :mode (("\\.l?hs$" . haskell-mode)
		 ("\\.cabal$" . haskell-cabal-mode))
  :config
  (use-package ghc
    :config

    ;; ghc-mod setting
    (autoload 'ghc-init "ghc" nil t)
    (autoload 'ghc-debug "ghc" nil t)
    (add-hook 'haskell-mode-hook (lambda () (ghc-init))))

  (setq haskell-process-type 'stack-ghci)
  (setq haskell-process-path-ghci "stack")
  (setq haskell-process-args-ghci "ghci")

  ;; エコーエリアに関数の型を表示するモードをオンにする
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

  ;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent))
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  ;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

  (defun my-haskell-mode-conf ()
    "Set haskell-mode config."
    (interactive-haskell-mode)
    (font-lock-replace-symbol mode "\\(->\\)" "→")
    (font-lock-replace-symbol mode "\\(<-\\)" "←")
    (font-lock-replace-symbol mode "\\(=>\\)" "⇒"))

  (add-hook 'haskell-mode-hook 'my-haskell-mode-conf)
  ;; (require 'mylib-keycombo)
  ;; (add-hook 'haskell-mode-hook 'my-key-combo-haskell-conf)
  )

(add-to-list 'auto-mode-alist '("\\.hamlet$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.lucius$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.julius$" . js2-mode))

(require 'align)
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

(provide '50_init-haskell)
;;; 50_init-haskell ends here

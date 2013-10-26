;;; 30_init-popup --- 30_init-popup
;; This program is free software
;;; Commentary:
;;; Code:
;(auto-install-from-url "https://github.com/m2ym/popwin-el/raw/master/popwin.el")
;(auto-install-from-url "http://www.emacswiki.org/emacs-en/PosTip")
(add-to-list 'load-path "~/.emacs.d/elisp")

(require '00_init-macro)
(req popwin)
(setq display-buffer-function 'popwin:display-buffer)

;;anything
(setq anything-samewindow nil)
(push '("*anything*" :height 15) popwin:special-display-config)
;;dired
(push '(dired-mode :position top) popwin:special-display-config)

;; Apropos
(push '("*slime-apropos*") popwin:special-display-config)
;; Macroexpand
(push '("*slime-macroexpansion*") popwin:special-display-config)
;; Help
(push '("*slime-description*") popwin:special-display-config)
;; Compilation
(push '("*slime-compilation*" :noselect t) popwin:special-display-config)
;; Cross-reference
(push '("*slime-xref*") popwin:special-display-config)
;; Debugger
(push '(sldb-mode :stick t) popwin:special-display-config)
;; REPL
(push '(slime-repl-mode) popwin:special-display-config)
;; Connections
(push '(slime-connection-list-mode) popwin:special-display-config)

;;@see http://d.hatena.ne.jp/sokutou-metsu/20110205/1296915272
(push '(" *auto-async-byte-compile*" :height 14 :position bottom :noselect t) popwin:special-display-config)
(push '("*VC-log*" :height 10 :position bottom) popwin:special-display-config)
;;Compile-Log
(push '("*Compile-Log*" :height 10 :noselect t) popwin:special-display-config)
(push '("*Process List*" :stick t) popwin:special-display-config)
(push '("*sdic*" :noselect t) popwin:special-display-config)
(push '("*init log*" :stick t) popwin:special-display-config)
(push '("\\*magit.*" :stick t :regexp t) popwin:special-display-config)
(push '("*compilation*" :regexp t) popwin:special-display-config)
(push '("*ert*" :regexp t) popwin:special-display-config)

(provide '30_init-popup)
;;; 30_init-popup ends here

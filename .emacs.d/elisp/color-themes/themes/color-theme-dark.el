(eval-when-compile
  (require 'color-theme))

(defun color-theme-dark ()
   "Color theme by Tomas Cerha, created 2001-11-13. color-theme-deep-blue"
   (interactive)
   (color-theme-install
    '(color-theme-dark
      ((background-color . "#191717")
       (background-mode . dark)
       (border-color . "black")
       (cursor-color . "green")
       (foreground-color . "#D2DEC4")
       (mouse-color . "white"))
      ((browse-kill-ring-separator-face . bold)
       (display-time-mail-face . mode-line)
       (help-highlight-face . underline)
       (list-matching-lines-face . secondary-selection)
       (vc-annotate-very-old-color . "#0046FF")
       (view-highlight-face . highlight)
       (widget-mouse-face . highlight))
      (default ((t (:stipple nil :background "#102e4e" :foreground "#eeeeee" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :family "September"))))

      (bold ((t (:bold t :weight bold :height 1.1))))
      (italic ((t (:italic t :slant italic :height 1.1))))
      (bold-italic ((t (:italic t :bold t :slant italic :weight bold :height 1.1))))
      (fixed-pitch ((t (:family "September"))))
      (variable-pitch ((t (:family "September"))))
      (underline ((t (:underline t))))
      (menu ((t (:background "gray" :foreground "black" :family "September"))))
      (border ((t (:background "black"))))
      (cursor ((t (:background "green" :foreground "black"))))
      (fringe ((t (:background "#405060"))))
      (mouse ((t (:background "white"))))
      (region ((t (:background "DarkCyan"))))
      (secondary-selection ((t (:background "yellow" :foreground "gray10"))))
      (scroll-bar ((t (:background "gray" :foreground "#506070"))))
      (tool-bar ((t (:background "grey75" :foreground "black" :box (:line-width 1 :style released-button)))))
      (tooltip ((t (:background "lightyellow" :foreground "black"))))
      (header-line ((t (:box (:line-width 2 :style released-button) :background "grey20" :foreground "grey90" :box nil))))

      (highlight ((t (:background "darkolivegreen"))))
      (highlight-current-line ((t (:background "#282626"))))

      ;; isearch
      (isearch ((t (:background "palevioletred2" :foreground "brown4"))))
      (isearch-lazy-highlight ((t (:background "paleturquoise4"))))

      ;; whitespcae
      (whitespace-space ((t (:family "IPAGothic" :foreground "aquamarine"))))
      ;; 行末半角スペース
      (trailing-whitespace ((t (:underline "SteelBlue"))))

      ;; info
      (Info-title-1-face ((t (:bold t :weight bold :family "September" :height 1.728))))
      (Info-title-2-face ((t (:bold t :family "September" :weight bold :height 1.44))))
      (Info-title-3-face ((t (:bold t :weight bold :family "September" :height 1.2))))
      (Info-title-4-face ((t (:bold t :family "September" :weight bold))))

      (info-header-node ((t (:foreground "DeepSkyBlue1"))))
      (info-header-xref ((t (:bold t :weight bold :foreground "SeaGreen2"))))
      (info-menu-5 ((t (:foreground "wheat"))))
      (info-menu-header ((t (:bold t :family "September" :weight bold))))
      (info-node ((t (:foreground "DeepSkyBlue1"))))
      (info-xref ((t (:bold t :foreground "SeaGreen2" :weight bold))))

      (change-log-acknowledgement-face ((t (:italic t :slant italic :foreground "CadetBlue"))))
      (change-log-conditionals-face ((t (:foreground "SeaGreen2"))))
      (change-log-date-face ((t (:foreground "burlywood"))))
      (change-log-email-face ((t (:foreground "SeaGreen2"))))
      (change-log-file-face ((t (:bold t :weight bold :foreground "goldenrod"))))
      (change-log-function-face ((t (:foreground "SeaGreen2"))))
      (change-log-list-face ((t (:bold t :weight bold :foreground "DeepSkyBlue1"))))
      (change-log-name-face ((t (:foreground "gold"))))

      (comint-highlight-input ((t (:bold t :weight bold))))
      (comint-highlight-prompt ((t (:foreground "cyan"))))

      (cvs-filename-face ((t (:foreground "lightblue"))))
      (cvs-handled-face ((t (:foreground "pink"))))
      (cvs-header-face ((t (:bold t :foreground "lightyellow" :weight bold))))
      (cvs-marked-face ((t (:bold t :foreground "green" :weight bold))))
      (cvs-msg-face ((t (:italic t :slant italic))))
      (cvs-need-action-face ((t (:foreground "orange"))))
      (cvs-unknown-face ((t (:foreground "red"))))

      (calendar-today-face ((t (:background "blue"))))
      (diary-face ((t (:foreground "orange red"))))
      (holiday-face ((t (:foreground "green"))))

      (diff-added ((t (:background "gray26" :foreground "LightGoldenrod1"))))
      (diff-removed ((t (:background "gray26" :foreground "deep sky blue"))))
      (diff-changed ((t (:foreground "DeepSkyBlue1"))))
      (diff-context ((t (:foreground "grey70"))))
      (diff-file-header ((t (:bold t :background "grey60" :foreground "yellow" :weight bold))))
      (diff-function ((t (:foreground "grey70"))))
      (diff-header ((t (:background "grey45"))))
      (diff-hunk-header ((t (:background "grey45" :foreground "light pink"))))
      (diff-index ((t (:bold t :weight bold :background "grey60" :foreground "thistle"))))
      (diff-nonexistent ((t (:bold t :weight bold :background "grey60"))))

      (font-latex-bold-face ((t (:bold t :foreground "OliveDrab" :weight bold))))
      (font-latex-italic-face ((t (:italic t :foreground "OliveDrab" :slant italic))))
      (font-latex-math-face ((t (:foreground "burlywood"))))
      (font-latex-sedate-face ((t (:foreground "LightGray"))))
      (font-latex-string-face ((t (:foreground "LightSalmon"))))
      (font-latex-warning-face ((t (:bold t :foreground "Pink" :weight bold))))

      (font-lock-builtin-face ((t (:foreground "LightCoral"))))
      (font-lock-comment-face ((t (:italic t :foreground "PaleGreen1" :slant italic))))
      (font-lock-constant-face ((t (:foreground "gold"))))
      (font-lock-doc-face ((t (:foreground "BlanchedAlmond"))))
      (font-lock-doc-string-face ((t (:foreground "BlanchedAlmond"))))
      (font-lock-function-name-face ((t (:bold t :foreground "khaki" :weight bold))))
      (font-lock-keyword-face ((t (:bold t :foreground "sky blue" :weight bold))))
      (font-lock-preprocessor-face ((t (:foreground "gold"))))
      (font-lock-reference-face ((t (:foreground "LightCoral"))))
      (font-lock-string-face ((t (:foreground "burlywood"))))
      (font-lock-type-face ((t (:foreground "CadetBlue1"))))
      (font-lock-variable-name-face ((t (:foreground "SeaGreen2"))))
      (font-lock-warning-face ((t (:foreground "yellow"))))

      (action-lock-face ((t (:bold t :foreground "cyan" :weight bold :underline t))))

      (mode-line ((t (:foreground "#bbbbbc" :background "#222222" :box (:line-width 1 :color nil :style released-button)))))
      (mode-line-inactive ((t (:foreground "#bbbbbc" :background "#555753"))))
      (mode-line-buffer-id ((t (:bold t :foreground "orange" :background nil))))

      (show-paren-match-face ((t (:foreground "#2e3436" :background "#73d216"))))
      (show-paren-mismatch-face ((t (:background "#ad7fa8" :foreground "#2e3436"))))

      (widget-button-face ((t (:bold t :weight bold))))
      (widget-button-pressed-face ((t (:foreground "red"))))
      (widget-documentation-face ((t (:foreground "lime green"))))
      (widget-field-face ((t (:background "dim gray"))))
      (widget-inactive-face ((t (:foreground "light gray"))))
      (widget-single-line-field-face ((t (:background "dim gray"))))

      ;; rst
      (rst-level-1-face ((t (:background "gray0" :foreground "white"))))
      (rst-level-2-face ((t (:background "gray3" :foreground "white"))))
      (rst-level-3-face ((t (:background "gray6" :foreground "white"))))
      (rst-level-4-face ((t (:background "gray9" :foreground "white"))))
      (rst-level-5-face ((t (:background "gray12" :foreground "white"))))
      (rst-level-6-face ((t (:background "gray15" :foreground "white"))))

      ;; howm
      ;;(howm-mode-title-face ((t (:background "grey15" :foreground "OliveDrab1"))))
      (howm-mode-title-face ((t (:background "gray0" :foreground "white"))))

      ;; vline
      (vline-visual ((t (:background "gray13"))))
      (vline ((t (:background "gray13"))))

      ;; nxml
      (nxml-element-local-name ((t (:foreground "SkyBlue1"))))

      ;; anything
      ;; ファイル色がデフォルト Blue だが背景が黒系統だと見にくいので変更
      (anything-file-name ((t (:foreground "Yellow"))))

      ;; 変更点のハイライト
      (highlight-changes ((t (:foreground nil :background "#382f2f"))))
      (highlight-changes-delete ((t (:foreground nil :background "#916868"))))

      ;; flymake
      (flymake-errline ((t (:background "red4"))))
      (flymake-warnline ((t (:background "dark slate blue"))))

      )))
(provide 'color-theme-dark)
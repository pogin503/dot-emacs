;;; 01_init-font.el --- set font config
;;; Commentary:
;;; Code:
(eval-when-compile
  (require '00_init-hanbetu))

(if (not run-no-window)
    (if run-windows
        ;; (set-face-attribute 'default nil
        ;;                   :family "Meiryo"
        ;;                   :height 100
        ;;                   :weight 'bold
        ;;                   ;; :weight 'normal
        ;;                   )
        (set-face-attribute 'default nil
                    :family "Ricty Discord"
                    :height 100
                    :weight 'bold)
      (set-fontset-font nil 'japanese-jisx0208
                        (font-spec :family "Meiryo")))
  (if run-linux
      (progn
        (set-face-attribute 'default nil
                            :family "Ricty"
                            :height 110
                            :weight 'bold)
        (set-fontset-font nil 'japanese-jisx0208
                          (font-spec :family "Ricty")))))

(if (not run-no-window)
    (when run-darwin
      ;;; フレームのフォントを設定
      (let* ((size 10) ; ASCIIフォントのサイズ [9/10/12/14/15/17/19/20/...]
             (asciifont "Menlo") ; ASCIIフォント
             (jpfont "Hiragino Maru Gothic ProN") ; 日本語フォント
             (h (* size 10))
             (fontspec (font-spec :family asciifont))
             (jp-fontspec (font-spec :family jpfont)))
        (set-face-attribute 'default nil :family asciifont :height h)
        (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
        (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
        (set-fontset-font nil 'katakana-jisx0201 jp-fontspec) ; 半角カナ
        (set-fontset-font nil '(#x0080 . #x024F) fontspec) ; 分音符付きラテン
        (set-fontset-font nil '(#x0370 . #x03FF) fontspec) ; ギリシャ文字
        )
      ;;; フォントサイズの比を設定
      (dolist (elt '(("^-apple-hiragino.*" . 1.2)
                     (".*osaka-bold.*" . 1.2)
                     (".*osaka-medium.*" . 1.2)
                     (".*courier-bold-.*-mac-roman" . 1.0)
                     (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
                     (".*monaco-bold-.*-mac-roman" . 0.9)))
        (add-to-list 'face-font-rescale-alist elt))))
;;; 01_init-font.el ends here

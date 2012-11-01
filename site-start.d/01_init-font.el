;;; 01_init-font.el --- set font config

(if run-windows
    (progn
      (set-face-attribute 'default nil
                          :family "Meiryo"
                          :height 100
                          :weight 'bold
                          ;; :weight 'normal
                          )
      (set-fontset-font nil 'japanese-jisx0208
                        (font-spec :family "Meiryo")))
  (progn
      (set-face-attribute 'default nil
                          :family "Ricty"
                          :height 110
                          :weight 'bold)
      (set-fontset-font nil 'japanese-jisx0208
                        (font-spec :family "Ricty")))
  )

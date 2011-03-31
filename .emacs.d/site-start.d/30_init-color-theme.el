;;color-theme***********************************
;;@see http://sakito.jp/emacs/colortheme.html

(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-ntemacs)))

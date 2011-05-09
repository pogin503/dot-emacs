;(auto-install-from-url "http://github.com/kiwanami/emacs-deferred/raw/master/deferred.el")
;(auto-install-from-url "http://github.com/kiwanami/emacs-inertial-scroll/raw/master/inertial-scroll.el")

(require 'inertial-scroll)
(setq inertias-global-minor-mode-map 
      (inertias-define-keymap
       '(
         ("<next>"  . inertias-up)
         ("<prior>" . inertias-down)
         ("C-v"     . inertias-up)
         ("M-v"     . inertias-down)
         ) inertias-prefix-key))
(inertias-global-minor-mode 1)

(setq inertias-initial-velocity 70) ; 初速（大きいほど一気にスクロールする）
(setq inertias-friction 120)        ; 摩擦抵抗（大きいほどすぐ止まる）
(setq inertias-rest-coef 1.0)         ; 画面端でのバウンド量（0はバウンドしない。1.0で弾性反発）
(setq inertias-update-time 60)      ; 画面描画のwait時間（msec）


;; https://github.com/hayamiz/twittering-mode/

(add-to-list 'load-path "~/.emacs.d/plugins/twittering-mode")

(req twittering-mode)
(setq twittering-oauth-invoke-browser t)
(setq twittering-username "regluu503")
(setq twittering-use-master-password t)
(setq twittering-allow-insecure-server-cert t)
(setq twittering-display-remaining t)
;; (setq twittering-proxy-use t)
;; (setq twittering-proxy-server "127.0.0.1")
;; (setq twittering-proxy-port 8080)
;; (setq twittering-proxy-user "ユーザー名")
;; (setq twittering-proxy-password "パスワード")

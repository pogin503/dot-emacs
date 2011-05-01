;;INSTALL
;(install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/oddmuse.el")

;(require 'oddmuse)
;; if needed
;(setq url-proxy-services '(("http" . "your.proxy.host:portnumber")))
;(oddmuse-mode-initialize)

;(add-hook 'oddmuse-mode-hook
;          (lambda ()
;            (unless (string-match "question" oddmuse-post)
;              (when (string-match "EmacsWiki" oddmuse-wiki)
;                (setq oddmuse-post (concat "uihnscuskc=1;" oddmuse-post)))
;              (when (string-match "OddmuseWiki" oddmuse-wiki)
;                (setq oddmuse-post (concat "ham=1;" oddmuse-post))))))

;(auto-install-from-url http://www.emacswiki.org/emacs/download/yaoddmuse.el)

(require 'yaoddmuse)
;(yaoddmuse-update-pagename t)


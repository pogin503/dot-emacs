;;; .spacemacs --- .spacemacs -*- lexical-binding: t; coding: utf-8 -*-
;;; Commentary:
;; This program is free software
;;; Code:

;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     lua
     ansible
     csv
     nginx
     sql
     go
     python
     javascript
     yaml
     shell-scripts
     html
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     helm
     ;; auto-completion
     ;; better-defaults
     emacs-lisp
     git
     markdown
     org
     haskell
     php
     ruby
     (rust :disbled-for rust)
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     ;; spell-checking
     ;; syntax-checking
     ;; version-control
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(
                                      ag
                                      ;; アナフォリックマクロ用
                                      anaphora
                                      company
                                      company-tabnine
                                      docker
                                      dockerfile-mode
                                      drag-stuff
                                      editorconfig
                                      flycheck-package
                                      free-keys
                                      ;; gtags用
                                      ggtags
                                      git-timemachine
                                      gitter
                                      helm-gtags
                                      helpful
                                      markdown-preview-mode
                                      multiple-cursors
                                      ;; 不要ファイルを管理してくれるやつ
                                      no-littering
                                      nyan-mode
                                      paredit
                                      peep-dired
                                      pipenv
                                      quickrun
                                      rustic
                                      sequential-command
                                      tabbar
                                      treemacs
                                      treemacs-projectile
                                      visual-regexp
                                      visual-regexp-steroids
                                      vue-mode
                                      wgrep-ag
                                      f
                                      s
                                      lsp-mode
                                      lsp-ui
                                      lsp-pyright
                                      leaf
                                      leaf-keywords
                                      )

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(
                                    org-projectile
                                    )
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'emacs
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   dotspacemacs-mode-line-theme 'spacemacs
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("UDEV Gothic"
                               :size 16
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  ;; WhichKeyの古い関数をSpacemacsで使っている
(defalias 'which-key-declare-prefixes 'which-key-add-key-based-replacements)
(make-obsolete 'which-key-declare-prefixes
              'which-key-add-key-based-replacements
              "2016-10-05")
(defalias 'which-key-declare-prefixes-for-mode
 'which-key-add-major-mode-key-based-replacements)
(make-obsolete 'which-key-declare-prefixes-for-mode
              'which-key-add-major-mode-key-based-replacements
              "2016-10-05")
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
  (setq-default dotspacemacs-configuration-layers
                '((syntax-checking :variables syntax-checking-enable-tooltips t)))

  (defun add-to-load-path (&rest paths)
    "Add to load path recursively.
`PATHS' Directorys you want to read recursively."
    (let (path)
      (dolist (path paths paths)
        (let ((default-directory (expand-file-name (locate-user-emacs-file path))))
          (add-to-list 'load-path default-directory)
          (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
              (normal-top-level-add-subdirs-to-load-path))))))

  (when (file-exists-p (expand-file-name (concat "/Users/" user-login-name "/workspace/dot-emacs/")))
    (add-to-load-path (concat "/Users/" user-login-name "/workspace/dot-emacs/etc")
                      (concat "/Users/" user-login-name "/workspace/dot-emacs/elisp")
                      (concat "/Users/" user-login-name "/workspace/dot-emacs/site-start.d")
                      ))

  (require 'use-package)
  (setq byte-compile-warnings '(not cl-functions obsolete))

  (use-package 01_init-global)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
  ;; (use-package dired+
  ;;   :config
  ;;   (diredp-toggle-find-file-reuse-dir 1)
  ;;   ;; (diredp-mouse-find-file-reuse-dir-buffer 1)
  ;;   (diredp-make-find-file-keys-reuse-dirs)
  ;;   (put 'dired-find-alternate-file 'disabled nil)
  ;;   )
  (setq-default c-basic-offset 4       ; 基本インデント量
                tab-width 4            ; タブ幅
                ;; indent-tabs-mode nil
                )  ; インデントをタブでするかスペースでするか


  (use-package mylib)
  (use-package 00_init-vars)
  (use-package 00_init-package)
  (use-package 01_init-encoding)
  (use-package 01_init-keybind)
  (use-package 30_init-helm)
  (use-package 30_init-autoinsert)
  (use-package 30_init-treemacs)
  (use-package 50_init-html)
  (use-package 50_init-ruby)
  (use-package 50_init-rust)
  ;; path
  (exec-path-from-shell-initialize)
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(alert-default-style 'notifier)
 '(alert-severity-colors
   '((urgent . "red")
     (high . "orange")
     (moderate . "yellow")
     (normal . "grey85")
     (low . "blue")
     (trivial . "purple")))
 '(backup-by-copying nil)
 '(blink-cursor-mode nil)
 '(c-electric-pound-behavior '(alignleft))
 '(column-number-mode t)
 '(company-minimum-prefix-length 4)
 '(company-tabnine-always-trigger nil)
 '(company-tabnine-wait 0.3)
 '(delete-auto-save-files t)
 '(delete-old-versions t)
 '(evil-want-Y-yank-to-eol nil)
 '(flycheck-emacs-lisp-load-path load-path)
 '(helm-M-x-always-save-history t)
 '(helm-adaptive-history-file (locate-user-emacs-file ".cache/helm-adaptive-history"))
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-path-style 'relative)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-ignore-extensions t)
 '(ido-save-directory-list-file (locate-user-emacs-file ".cache/ido.last"))
 '(ido-use-faces nil)
 '(ido-use-filename-at-point 'guess)
 '(inf-ruby-default-implementation "pry")
 '(inf-ruby-eval-binding "Pry.toplevel_binding" t)
 '(kept-new-versions 5)
 '(kept-old-versions 5)
 '(lsp-rust-unstable-features t)
 '(make-backup-files nil)
 '(markdown-command "grip")
 '(my-user-name "pogin503" t)
 '(org-agenda-files nil)
 '(package-selected-packages
   '(dockerfile-mode leaf-tree leaf-convert undo-tree queue treemacs-projectile treemacs cfrs pfuture posframe tabbar rvm rspec-mode robe racer lsp-ui rustic use-package leaf typescript-mode lua-mode lsp-pyright org-plus-contrib pipenv package-lint-flymake company-tabnine which-key web-mode pos-tip rust-mode solarized-theme init-loader nyan-mode drag-stuff jinja2-mode ansible-doc ansible latex-pretty-symbols visual-regexp-steroids visual-regexp yasnippet-snippets auto-yasnippet whitespace-cleanup-mode tramp-term counsel-tramp tide ssh-config-mode ssh restclient virtualenv py-autopep8 company-jedi jedi elpy prettier-js php-refactor-mode counsel-projectile pdf-tools pandoc-mode ox-pandoc interleave nov nix-sandbox nix-mode multi-term term+ markdown-preview-mode javadoc-lookup ivy swiper smex ivy-rich itail import-js geben highlight-symbol hardcore-mode guess-language groovy-mode groovy-imports google-maps flycheck-inline eslintd-fix ensime scala-mode sbt-mode elm-mode flycheck-elm elm-yasnippets ecb cmake-ide counsel-bbdb epkg no-littering csv-mode srefactor cmake-mode gitter mode-line-bell git-gutter nginx-mode sql-indent ein go-guru go-eldoc go-mode hyperbole rope-read-mode less-css-mode sequential-command flycheck-package package-lint docker yapfify pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode anaconda-mode free-keys zenburn-theme ggtags helm-gtags peep-dired web-beautify livid-mode skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor js2-mode js-doc coffee-mode diredful yaml-mode insert-shebang fish-mode vue-mode ruby-tools ruby-test-mode rubocop rbenv rake phpunit phpcbf php-extras php-auto-yasnippets org-projectile org-category-capture minitest drupal-mode php-mode chruby bundler inf-ruby helpful flycheck-rust wgrep-ag multiple-cursors quickrun ag tagedit slim-mode scss-mode sass-mode pug-mode helm-css-scss haml-mode emmet-mode editorconfig paredit anaphora ws-butler winum volatile-highlights vi-tilde-fringe uuidgen toml-mode toc-org spaceline smeargle restart-emacs rainbow-delimiters popwin persp-mode pcre2el paradox orgit org-present org-pomodoro org-mime org-download org-bullets open-junk-file neotree move-text mmm-mode markdown-toc magit-gitflow macrostep lorem-ipsum linum-relative link-hint intero indent-guide hungry-delete htmlize hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-hoogle helm-gitignore helm-flx helm-descbinds helm-ag haskell-snippets google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu elisp-slime-nav dumb-jump diminish define-word company-ghci company-ghc column-enforce-mode cmm-mode clean-aindent-mode cargo auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line))
 '(recentf-auto-cleanup 600)
 '(recentf-exclude
   '(".recentf" "/elpa/" "/elisps/" "^/tmp/" "/\\.git/" "/\\.cask/" "\\.mime-example" "\\.ido.last" "woman_cache.el" "COMMIT_EDITMSG" "MERGE_MSG" "bookmarks" "\\.gz$" "Command attempt to use minibuffer while in minibuffer"))
 '(recentf-max-saved-items 2000)
 '(ruby-insert-encoding-magic-comment nil)
 '(safe-local-variable-values
   '((eval setq-local flycheck-command-wrapper-function
           (lambda
             (command)
             (let
                 ((default-directory
                    (find-git-root)))
               (append
                '("bundle" "exec")
                command))))
     (eval let
           ((default-directory
              (find-git-root)))
           (lambda
             (command)
             (let
                 ((default-directory
                    (find-git-root)))
               (append
                '("bundle" "exec")
                command))))
     (no-byte-compile t)))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(truncate-partial-width-windows 0)
 '(url-history-file "~/.emacs.d/data/url/history")
 '(use-package-enable-imenu-support t)
 '(use-package-verbose t)
 '(vc-make-backup-files nil)
 '(version-control nil)
 '(warning-suppress-types '((use-package) (use-package))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "nil" :slant normal :weight normal :height 130 :width normal)))))


;;; .spacemacs ends here

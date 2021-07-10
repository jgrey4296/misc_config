;;; completion/ivy/+vars.el -*- lexical-binding: t; -*-

(after! (ivy)
  (setq ivy-height 17
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        projectile-completion-system 'ivy
        ;; disable magic slash on non-match
        ivy-magic-slash-non-match-action nil
        ;; don't show recent files in switch-buffer
        ivy-use-virtual-buffers nil
        ;; ...but if that ever changes, show their full path
        ivy-virtual-abbreviate 'full
        ;; don't quit minibuffer on delete-error
        ivy-on-del-error-function #'ignore
        ;; enable ability to select prompt (alternative to `ivy-immediate-done')
        ivy-use-selectable-prompt t
        ivy-rich-parse-remote-buffer nil
        swiper-action-recenter t)
)
(after! (helm helm-files)
  (setq  helm-candidate-number-limit 50
         helm-truncate-lines t
         ;; Remove extraineous helm UI elements
         helm-display-header-line nil
         helm-mode-line-string nil
         helm-ff-auto-update-initial-value nil
         helm-find-files-doc-header nil
         ;; Default helm window sizes
         helm-display-buffer-default-width nil
         helm-display-buffer-default-height 0.25
         ;; When calling `helm-semantic-or-imenu', don't immediately jump to
         ;; symbol at point
         helm-imenu-execute-action-at-once-if-one nil
         ;; disable special behavior for left/right, M-left/right keys.
         helm-ff-lynx-style-map nil
         ;; Matching
         ;; helm-apropos-fuzzy-match     fuzzy
         ;; helm-bookmark-show-location  fuzzy
         ;; helm-buffers-fuzzy-matching  fuzzy
         ;; helm-ff-fuzzy-matching       fuzzy
         ;; helm-file-cache-fuzzy-match  fuzzy
         ;; helm-flx-for-helm-locate     fuzzy
         ;; helm-imenu-fuzzy-match       fuzzy
         ;; helm-lisp-fuzzy-completion   fuzzy
         ;; helm-locate-fuzzy-match      fuzzy
         ;; helm-projectile-fuzzy-match  fuzzy
         ;; helm-recentf-fuzzy-match     fuzzy
         ;; helm-semantic-fuzzy-match    fuzzy
         helm-boring-file-regexp-list (append (list "\\.projects$" "\\.DS_Store$") helm-boring-file-regexp-list)
        ;; helm-boring-buffer-regexp-list
        )
  )

(setq company-idle-delay 1)

(setq +file-templates-dir "/Volumes/documents/github/emacs_files/snippets/file-templates")

(defvar +file-templates-alist
  '(;; General
    (gitignore-mode)
    (dockerfile-mode)
    ("/docker-compose\\.yml$" :mode yaml-mode)
    ("/Makefile$"             :mode makefile-gmake-mode)
    ;; elisp
    ("/.dir-locals.el$")
    ("/packages\\.el$" :when +file-templates-in-emacs-dirs-p
     :trigger "__doom-packages"
     :mode emacs-lisp-mode)
    ("/doctor\\.el$" :when +file-templates-in-emacs-dirs-p
     :trigger "__doom-doctor"
     :mode emacs-lisp-mode)
    ("/test/.+\\.el$" :when +file-templates-in-emacs-dirs-p
     :trigger "__doom-test"
     :mode emacs-lisp-mode)
    ("\\.el$" :when +file-templates-in-emacs-dirs-p
     :trigger "__doom-module"
     :mode emacs-lisp-mode)
    ("-test\\.el$" :mode emacs-ert-mode)
    (emacs-lisp-mode :trigger "__package")
    (snippet-mode)
    ;; C/C++
    ("/main\\.c\\(?:c\\|pp\\)$"   :trigger "__main.cpp"    :mode c++-mode)
    ("/win32_\\.c\\(?:c\\|pp\\)$" :trigger "__winmain.cpp" :mode c++-mode)
    ("\\.c\\(?:c\\|pp\\)$"        :trigger "__cpp" :mode c++-mode)
    ("\\.h\\(?:h\\|pp\\|xx\\)$"   :trigger "__hpp" :mode c++-mode)
    ("\\.h$" :trigger "__h" :mode c-mode)
    (c-mode  :trigger "__c")
    ;; direnv
    ("/\\.envrc$" :trigger "__envrc" :mode direnv-envrc-mode)
    ;; go
    ("/main\\.go$" :trigger "__main.go" :mode go-mode :project t)
    (go-mode :trigger "__.go")
    ;; web-mode
    ("/normalize\\.scss$" :trigger "__normalize.scss" :mode scss-mode)
    ("/master\\.scss$" :trigger "__master.scss" :mode scss-mode)
    ("\\.html$" :trigger "__.html" :mode web-mode)
    (scss-mode)
    ;; java
    ("/main\\.java$" :trigger "__main" :mode java-mode)
    ("/build\\.gradle$" :trigger "__build.gradle" :mode android-mode)
    ("/src/.+\\.java$" :mode java-mode)
    ;; javascript
    ("/package\\.json$"        :trigger "__package.json" :mode json-mode)
    ("/bower\\.json$"          :trigger "__bower.json" :mode json-mode)
    ("/gulpfile\\.js$"         :trigger "__gulpfile.js" :mode js-mode)
    ("/webpack\\.config\\.js$" :trigger "__webpack.config.js" :mode js-mode)
    ("\\.js\\(?:on\\|hintrc\\)$" :mode json-mode)
    ;; Lua
    ("/main\\.lua$" :trigger "__main.lua" :mode love-mode)
    ("/conf\\.lua$" :trigger "__conf.lua" :mode love-mode)
    ;; Markdown
    (markdown-mode)
    ;; Markdown
    (nxml-mode)
    ;; Nix
    ("/shell\\.nix$" :trigger "__shell.nix")
    (nix-mode)
    ;; Org
    ("/README\\.org$"
     :when +file-templates-in-emacs-dirs-p
     :trigger "__doom-readme"
     :mode org-mode)
    (org-journal-mode :ignore t)
    (org-mode)
    ;; PHP
    ("\\.class\\.php$" :trigger "__.class.php" :mode php-mode)
    (php-mode)
    ;; Python
    ;; TODO ("tests?/test_.+\\.py$" :trigger "__" :mode nose-mode)
    ;; TODO ("/setup\\.py$" :trigger "__setup.py" :mode python-mode)
    (python-mode)
    ;; Ruby
    ("/lib/.+\\.rb$"      :trigger "__module"   :mode ruby-mode :project t)
    ("/spec_helper\\.rb$" :trigger "__helper"   :mode rspec-mode :project t)
    ("_spec\\.rb$"                              :mode rspec-mode :project t)
    ("/\\.rspec$"         :trigger "__.rspec"   :mode rspec-mode :project t)
    ("\\.gemspec$"        :trigger "__.gemspec" :mode ruby-mode :project t)
    ("/Gemfile$"          :trigger "__Gemfile"  :mode ruby-mode :project t)
    ("/Rakefile$"         :trigger "__Rakefile" :mode ruby-mode :project t)
    (ruby-mode)
    ;; Rust
    ("/Cargo.toml$" :trigger "__Cargo.toml" :mode rust-mode)
    ("/main\\.rs$" :trigger "__main.rs" :mode rust-mode)
    ;; Slim
    ("/\\(?:index\\|main\\)\\.slim$" :mode slim-mode)
    ;; Shell scripts
    ("\\.zunit$" :trigger "__zunit" :mode sh-mode)
    (fish-mode)
    (sh-mode)
    ;; Solidity
    (solidity-mode :trigger "__sol"))
  "An alist of file template rules. The CAR of each rule is either a major mode
symbol or regexp string. The CDR is a plist. See `set-file-template!' for more
information.")

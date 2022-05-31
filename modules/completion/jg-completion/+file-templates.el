;;; +file-templates.el -*- lexical-binding: t; -*-

(message "Setting file templates dir")
(setq +file-templates-dir "/Volumes/documents/github/emacs_files/snippets/file-templates")
(setq yas-snippet-dirs '(+snippets-dir doom-snippets-dir +file-templates-dir yasnippet-snippets-dir))
(setq yas--default-user-snippets-dir yas-snippet-dirs)

(setq +file-templates-alist
      '(;; General
        (gitignore-mode)
        (dockerfile-mode)
        ("/docker-compose\\.yml$" :mode yaml-mode)
        ("/Makefile$"             :mode makefile-gmake-mode)
        (snippet-mode)
        ;; direnv
        ("/\\.envrc$" :trigger "__envrc" :mode direnv-envrc-mode)
        ;; java
        ("/main\\.java$"    :trigger "__main"         :mode java-mode)
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
        (sh-mode)
        ;; Solidity
        (solidity-mode :trigger "__sol"))
      )

;; NOTE: Order from lowest - highest priority
;; Org
(set-file-templates!
 '("/README\\.org$" :when +file-templates-in-emacs-dirs-p :trigger "__doom-readme" :mode org-mode)
 '(org-journal-mode :ignore t)
 '(org-mode)
 )

;; Python
(set-file-templates!
 '(python-mode     :trigger "__")
 '("test_.+\\.py$" :trigger "__tests" :mode python-mode)
 '("_.+\\.py$"     :trigger "__"      :mode python-mode)
 '("\\.py$"        :trigger "__"      :mode python-mode)
 )


;; Lisp
(set-file-templates!
 '("/.dir-locals.el$")
 '("-test\\.el$" :mode emacs-ert-mode)
 '(emacs-lisp-mode :trigger "__package")
 '("\\.el$" :when +file-templates-in-emacs-dirs-p :trigger "__doom-module" :mode emacs-lisp-mode)
 '("packages\\.el$" :when +file-templates-in-emacs-dirs-p :trigger "__doom_packages" :mode emacs-lisp-mode)
 '("config\\.el$"   :when +file-templates-in-emacs-dirs-p :trigger "__doom_config"   :mode emacs-lisp-mode)
 '("/doctor\\.el$"  :when +file-templates-in-emacs-dirs-p :trigger "__doom-doctor"   :mode emacs-lisp-mode)
 '("/test/.+\\.el$" :when +file-templates-in-emacs-dirs-p :trigger "__doom-test"     :mode emacs-lisp-mode)
 )

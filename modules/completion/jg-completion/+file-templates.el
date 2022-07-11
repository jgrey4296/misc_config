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
        ;; Markdown
        (markdown-mode)
        ;; Markdown
        (sh-mode)
      )
)

;; NOTE: Order from lowest - highest priority
;; Org
(set-file-templates!
 '("/README\\.org$"       :trigger "__doom-readme"            :mode org-mode :when +file-templates-in-emacs-dirs-p )
 '("contact\\.org$"       :trigger "__contact"                :mode org-mode)
 '("invoice\\.org$"       :trigger "__invoice"                :mode org-mode)
 '("project\\.org$"       :trigger "__project"                :mode org-mode)
 '("design_doc\\.org$"    :trigger "__designDocNotes"         :mode org-mode)
 '("inst_pipeline\\.org$" :trigger "__institution_pipeline"   :mode org-mode)
 '("lit_review\\.org$"    :trigger "__lit_review"             :mode org-mode)
 '("two_pager\\.org$"     :trigger "__pacheco_vega_two_pager" :mode org-mode)

 '(org-journal-mode :ignore t)
 '(org-mode)
 )

;; Python
(set-file-templates!
 '(python-mode       :trigger "__")
 '("\\.py$"          :trigger "__"               :mode python-mode)
 '("setup\\.py$"     :trigger "__setup"          :mode python-mode)
 '("conf\\.py$"      :trigger "__conf"           :mode python-mode)
 '("_.+\\.py$"       :trigger "__"               :mode python-mode)
 '("test_.+\\.py$"   :trigger "__tests"          :mode python-mode)
 '("__init__\\.py$"  :trigger "__init"           :mode python-mode)
 '("setup\\.cfg$"    :trigger "__setup_cfg"      :mode python-mode)
 '("pyproject.toml$" :trigger "__pyproject_toml" :mode python-mode)
 '("LICENSE$"        :trigger "__license-acab"   :mode text-mode)
 )


;; Lisp
(set-file-templates!
 '(emacs-lisp-mode    :trigger "__package")
 '("\\.el$" :when +file-templates-in-emacs-dirs-p :trigger "__doom-module" :mode emacs-lisp-mode)
 '("-test\\.el$" :mode emacs-ert-mode)
 '("/.dir-locals.el$" :mode emacs-lisp-mode :trigger "__dir_locals")
 '("mode\\.el$"       :trigger "__mode" :mode emacs-lisp-mode)
 '("minor-mode\\.el$" :trigger "__minor-mode" :mode emacs-lisp-mode)
 '("packages\\.el$" :when +file-templates-in-emacs-dirs-p :trigger "__doom_packages" :mode emacs-lisp-mode)
 '("config\\.el$"   :when +file-templates-in-emacs-dirs-p :trigger "__doom_config"   :mode emacs-lisp-mode)
 '("/doctor\\.el$"  :when +file-templates-in-emacs-dirs-p :trigger "__doom-doctor"   :mode emacs-lisp-mode)
 '("/test/.+\\.el$" :when +file-templates-in-emacs-dirs-p :trigger "__doom-test"     :mode emacs-lisp-mode)
 )

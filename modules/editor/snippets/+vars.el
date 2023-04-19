;;; +vars.el -*- lexical-binding: t; -*-


;; Reduce default verbosity. 3 is too chatty about initializing yasnippet. 2
;; is just right (only shows errors).
(defvar yas-verbosity 2)

(defvar +snippets--smartparens-enabled-p t)
(defvar +snippets--expanding-p nil)

(defvar +snippets-dir                     (expand-file-name "templates/" doom-user-dir) "Directory where `yasnippet' will search for your private snippets.")
(defvar jg-snippets-code-templates-dir    (expand-file-name "templates/code" doom-user-dir))
(defvar jg-snippets-file-templates-dir    (expand-file-name "templates/files" doom-user-dir))
(defvar jg-snippets-project-templates-dir (expand-file-name "templates/projects" doom-user-dir))
(defvar +file-templates-dir nil)
(defvar +file-templates-default-trigger "__")
(defvar +file-templates-inhibit nil)

(spec-handling-add! file-templates nil
                    ('general
                     ("/docker-compose\\.yml$" :mode yaml-mode)
                     ;; direnv
                     ("/\\.envrc$" :trigger "__envrc" :mode direnv-envrc-mode)
                     ;; Markdown
                     (markdown-mode)
                     ;; Markdown
                     (sh-mode :priority -100)
                     (gitignore-mode :priority -100)
                     (dockerfile-mode)
                     (snippet-mode)
                     )
 )

(defvar jg-snippet-dirs '(+snippets-dir yasnippet-snippets-dir))

(spec-handling-setq! snippets
                     +file-templates-dir jg-snippets-file-templates-dir
                     +snippets-dir       jg-snippets-code-templates-dir
                     yas-snippet-dirs    (-filter #'identity (append (list jg-snippets-code-templates-dir jg-snippets-file-templates-dir)
                                                                     jg-snippet-dirs))
                     yas--default-user-snippets-dir jg-snippets-code-templates-dir
                     )

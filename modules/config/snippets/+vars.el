;;; +vars.el -*- lexical-binding: t; -*-

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

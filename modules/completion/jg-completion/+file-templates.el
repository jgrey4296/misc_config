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


(provide 'jg-file-templates)

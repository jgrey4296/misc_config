;;; +file-templates.el -*- lexical-binding: t; -*-

(message "Setting file templates dir")

;; From Highest -> lowest priority
(+jg-completion-add-file-templates
 'general
 '(("/docker-compose\\.yml$" :mode yaml-mode)
   ("/Makefile$"             :mode makefile-gmake-mode)
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
   (gitignore-mode)
   (dockerfile-mode)
   (snippet-mode)
   )
 )

(+jg-completion-activate-file-templates)
(provide 'jg-file-templates)

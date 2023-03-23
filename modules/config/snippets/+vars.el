;;; +vars.el -*- lexical-binding: t; -*-

;;-- file specs
(after! jg-ui-reapply-hook-ready
  (+jg-snippets-add-file-spec
   'general
   '(("/docker-compose\\.yml$" :mode yaml-mode)
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
  )
;;-- end file specs

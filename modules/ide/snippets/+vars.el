;;; +vars.el -*- lexical-binding: t; -*-


;; default snippets library, if available
(add-to-list 'load-path jg-snippets-code-templates-dir)
(setq yas-indent-line 'fixed
      abbrev-file-name  (expand-file-name "tempalates/abbrevs_defs" doom-user-dir)
      )

(spec-handling-add! file-templates
                    '(general
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

(spec-handling-setq! snippets
                     +file-templates-dir jg-snippets-file-templates-dir
                     +snippets-dir       jg-snippets-code-templates-dir
                     yas-snippet-dirs    (-filter #'identity (append (list jg-snippets-code-templates-dir jg-snippets-file-templates-dir) jg-snippet-dirs))
                     yas--default-user-snippets-dir jg-snippets-code-templates-dir
                     yas-prompt-functions '(+jg-snippets-yas-prompt-fn)
                     )

(spec-handling-add! company
                    '(yas-minor-mode (:back company-yasnippet))
                    )

(spec-handling-add! auto-modes
                    '(snippets
                      ("templates/code/.+\\'"           . snippet-mode)
                      ("templates/general-insert/.+\\'" . fundamental-mode)
                      ("templates/lookup-regular/.+\\'" . fundamental-mode)
                      ("templates/company-dicts/.+\\'"  . fundamental-mode)
                      ("templates/files/.+\\'"          . snippet-mode)
                      )
                    )

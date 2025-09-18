;;; +vars.el -*- lexical-binding: t; -*-

;; default snippets library, if available
(add-to-list 'load-path jg-snippets-code-templates-dir)
(setq yas-indent-line 'fixed
      abbrev-file-name  (expand-file-name "abbrevs.defs" templates-loc)
      )

(speckler-add! file-templates ()
  '(general
    ("checklist.md\\'" :mode markdown-mode :trigger "__checklist")
    ("/docker-compose\\.yml\\'" :mode yaml-mode)
    (sh-mode :priority -100)
    (gitignore-mode :priority -100)
    (dockerfile-mode)
    (snippet-mode)
    ("." :mode prog-mode :trigger "__" :priority -200)
    )
  )

(speckler-setq! snippets ()
  +file-templates-dir jg-snippets-file-templates-dir
  +snippets-dir       jg-snippets-code-templates-dir
  yas-snippet-dirs    (-filter #'identity (append (list jg-snippets-code-templates-dir jg-snippets-file-templates-dir) jg-snippet-dirs))
  yas--default-user-snippets-dir jg-snippets-code-templates-dir
  yas-prompt-functions (list #'+jg-snippets-str-prompt-fn #'+jg-snippets-yas-prompt-fn)
  yas-triggers-in-field t
  )

(speckler-add! company ()
  '(yas-minor-mode (:last company-yasnippet))
  )

(speckler-add! auto-modes ()
  '(snippets
    ("templates/code/.+\\'"              . snippet-mode)
    ("templates/general-insert/.+\\'"    . fundamental-mode)
    ("templates/librarian-regular/.+\\'" . fundamental-mode)
    ("templates/company-dicts/.+\\'"     . fundamental-mode)
    ("templates/files/.+\\'"             . snippet-mode)
    )
  )

(speckler-add! popup ()
  '(snippets
    ("^untitled-snippet$" :side bottom :ttl 5 :height 0.4 :quit t :select t   :priority 50)
    ("^\\*Abbrevs\\*"     :side right  :ttl 5 :width 100  :quit t :select nil :priority 50)
    )
  )

;;; lang/idris/config.el -*- lexical-binding: t; -*-

(use-package! idris-mode
  :defer t
  :config
  (add-hook 'idris-mode-hook #'turn-on-idris-simple-indent)
  (when (modulep! +lsp)
    (add-hook 'idris-mode-hook #'lsp! 'append))
  (set-repl-handler! 'idris-mode 'idris-pop-to-repl)
  (set-lookup-handlers! 'idris-mode
    :documentation #'idris-docs-at-point)
  (map! :localleader
        :map idris-mode-map
        "r" #'idris-load-file
        "t" #'idris-type-at-point
        "d" #'idris-add-clause
        "l" #'idris-make-lemma
        "c" #'idris-case-split
        "w" #'idris-make-with-block
        "m" #'idris-add-missing
        "p" #'idris-proof-search
        "h" #'idris-docs-at-point)

  )

(spec-handling-add! lookup-regular nil
                    '(idris-mode
                      ("Idris Documentation" . "https://www.idris-lang.org/pages/documentation.html")
                      ("Idris Manual" . "https://idris2.readthedocs.io/en/latest/index.html")
                      )
                    )

;;; lang/jg-latex/config.el -*- lexical-binding: t; -*-

(use-package tex-mode
  :config
  (map! :after jg-bindings-total
        :map latex-mode-map
        :localleader
        :desc "Docs: Latex Wikibook" "0" (cmd! (browse-url "https://en.m.wikibooks.org/wiki/LaTeX"))
        )

  )

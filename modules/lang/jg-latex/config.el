;;; lang/jg-latex/config.el -*- lexical-binding: t; -*-

(use-package tex-mode
  :config
  (map! :after evil
        :map latex-mode-map
        :localleader
        :desc "Docs: Latex Wikibook" "0" (cmd! (+jg-browse-url "https://en.m.wikibooks.org/wiki/LaTeX"))
        )

  )

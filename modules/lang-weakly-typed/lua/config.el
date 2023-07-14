;;; lang/lua/config.el -*- lexical-binding: t; -*-

;; sp's default rules are obnoxious, so disable them
(provide 'smartparens-lua)

(load! "+vars")

;;
;;; Major modes

(use-package! lua-mode
  :defer t
  :init
  ;; lua-indent-level defaults to 3 otherwise. Madness.
  (setq lua-indent-level 2)
  :config

  (defvar +lua-lsp-dir (concat doom-data-dir "lsp/lua-language-server/")
    "Absolute path to the directory of sumneko's lua-language-server.
This directory MUST contain the 'main.lua' file and be the in-source build of
lua-language-server.")

  (add-hook 'lua-mode-local-vars-hook #'tree-sitter! 'append)

  )

(use-package! moonscript
  :when (modulep! +moonscript)
  :defer t
  :config
  (setq-hook! 'moonscript-mode-hook
    moonscript-indent-offset tab-width)
  (add-hook! 'moonscript-mode-hook
             #'+lua-moonscript-fix-single-quotes-h
             #'+lua-moonscript-fontify-interpolation-h)
  (when (modulep! :checkers syntax)
    (require 'flycheck-moonscript nil t)))

(use-package! fennel-mode
  :when (modulep! +fennel)
  :defer t
  :config
  (spec-handling-add! eval
                      `(fennel-mode :start ,#'fennel-repl)
                      )

  (setq-hook! 'fennel-mode-hook
    ;; To match the `tab-width' default for other lisp modes
    tab-width 2
    ;; Don't treat autoloads or sexp openers as outline headers, we have
    ;; hideshow for that.
    outline-regexp "[ \t]*;;;;* [^ \t\n]")

  (add-hook! 'fennel-mode-local-vars-hook 'tree-sitter! 'append)

  )

(def-project-mode! +lua-love-mode
  :modes '(moonscript-mode lua-mode markdown-mode json-mode)
  :when (+lua-love-project-root)
  :on-load
  (progn
    (map! :localleader
          :map +lua-love-mode-map
          "b" #'+lua/run-love-game))
  )

;;; +vars.el -*- lexical-binding: t; -*-

;;-- keymaps

(defvar jg-minibuffer-maps
  '(minibuffer-local-map
    minibuffer-local-ns-map
    minibuffer-local-completion-map
    minibuffer-local-must-match-map
    minibuffer-local-isearch-map
    evil-ex-completion-map
    read-expression-map
    read--expression-map
    )
  "A list of all core keymaps used for the minibuffer. adapted from evil-collection"
  )

(def-named-keymap! jg-minibuffer-ivy-map :sparse t)

(def-named-keymap! jg-minibuffer-local-map :sparse t)

(def-named-keymap! jg-minibuffer-read-expression-map :sparse t)

(def-named-keymap! jg-minibuffer-evil-ex-completion-map :sparse t)

(def-named-keymap! jg-minibuffer-evil-ex-search-keymap :sparse t)

;;-- end keymaps

(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt)

      enable-recursive-minibuffers t
      echo-keystrokes 0.02
      resize-mini-windows 'grow-only
      )

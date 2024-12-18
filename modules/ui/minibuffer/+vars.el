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

(defvar jg-minibuffer-ivy-map (make-sparse-keymap))

(defvar jg-minibuffer-local-map (make-sparse-keymap))

(defvar jg-minibuffer-read-expression-map (make-sparse-keymap))

(defvar jg-minibuffer-evil-ex-completion-map (make-sparse-keymap))

(defvar jg-minibuffer-evil-ex-search-keymap (make-sparse-keymap))

;;-- end keymaps

(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt)

      enable-recursive-minibuffers t
      echo-keystrokes 0.02
      resize-mini-windows 'grow-only
      )

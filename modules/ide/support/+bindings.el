;;; +bindings.el -*- lexical-binding: t; -*-

;;-- map defs

(defvar jg-lsp-mode-map           (make-sparse-keymap))

(defvar jg-lsp-command-map        (make-sparse-keymap))

(defvar jg-lsp-signature-mode-map (make-sparse-keymap))

(defvar jg-lsp-ui-imenu-mode-map  (make-sparse-keymap))

(defvar jg-lsp-ui-peek-mode-map   (make-sparse-keymap))

(defvar tree-sitter-mode-map (make-sparse-keymap))

(defvar +tree-sitter-inner-text-objects-map (make-sparse-keymap))

(defvar +tree-sitter-outer-text-objects-map (make-sparse-keymap))

(defvar +tree-sitter-goto-previous-map (make-sparse-keymap))

(defvar +tree-sitter-goto-next-map (make-sparse-keymap))

;;-- end map defs

(evil-make-overriding-map jg-lsp-mode-map)

(map! :leader
      :prefix "c"
      :desc "LSP"                                   "l"   jg-lsp-command-map
      :desc "LSP Code actions"                      "a"   #'lsp-avy-lens
      :desc "LSP Rename"                            "R"   #'lsp-rename
      :desc "List errors"                           "x"   #'flycheck-list-errors
      ;; :desc "jump to symbol in current workspace"   "j"   #'+jg-lsp-describe-workspace-symbol
      ;; :desc "Jump to symbol in any workspace"       "J"   #'lsp-ivy-global-workspace-symbol
      )

(map! :leader
      :after flycheck
      :prefix "c"
      :desc "Flycheck" "!" flycheck-command-map
      )

(map! :map jg-lsp-mode-map
      :n "g r" #'lsp-rename
      ;; :n "s '" #'lsp-ui-imenu
      :n "c x" #'flycheck-list-errors
      :n "s j" #'+jg-lsp-describe-workspace-symbol
      )

(map! :map jg-lsp-ui-imenu-mode-map
      :n "a"   #'+jg-lsp-imenu-visit
      :n "s"   #'+jg-lsp-imenu-visit

      :n "l"   #'lsp-ui-imenu--next-kind
      :n "h"   #'lsp-ui-imenu--prev-kind
      :n "K"   #'beginning-of-buffer
      :n "J"   #'end-of-buffer
      :n "RET" #'lsp-ui-imenu--view
      :n "q"   #'lsp-ui-imenu--kill
      :n "r"   #'lsp-ui-imenu--refresh
      )

(map! :map jg-lsp-browser-mode-map

      )

(map! :map jg-lsp-ui-peek-mode-map
      "j"   #'lsp-ui-peek--select-next
      "k"   #'lsp-ui-peek--select-prev
      "C-k" #'lsp-ui-peek--select-prev-file
      "C-j" #'lsp-ui-peek--select-next-file
      )

;;-- lsp commands
(map! :map jg-lsp-command-map
      (:prefix ("=" . "Formatting"))
      (:prefix ("r" . "Refactoring"))
      (:prefix ("a" . "Actions"))
      )

(map! :map jg-lsp-command-map ;; Formatting
      :prefix ("=" . "Formatting")
      :desc "format buffer" "=" #'lsp-format-buffer
      :desc "format region" "r" #'lsp-format-region
)

(map! :map jg-lsp-command-map ;; refactoring
      :prefix ("r" . "Refactoring")
      :desc "organize imports"             "o" #'lsp-organize-imports
      :desc "rename"                       "r" #'lsp-rename
      )

(map! :map jg-lsp-command-map ;; actions
      :prefix ("a" . "Actions")
      :desc "code actions"                 "a" #'lsp-execute-code-action
      :desc "highlight symbol"             "h" #'lsp-document-highlight
      :desc "lens"                         "l" #'lsp-avy-lens
      )

;;-- end lsp commands

;;-- semantic
(map! :map semantic-mode-map
      :after semantic
      :localleader
      :prefix ("^" . "Semantic")
      (:prefix ("t" . "toggle")
       :desc "Stick-func"     "s" #'semantic-stickyfunc-mode
       :desc "Highlight-func" "h" #'semantic-highlight-func-mode
       )

      )

;;-- end semantic

;;-- flycheck
(map! :map flycheck-error-list-mode-map
      "?" #'+jg-checkers-column-format
      :n "j"      #'flycheck-error-list-next-error
      :n "k"      #'flycheck-error-list-previous-error
      :n "RET"    #'flycheck-error-list-goto-error
      :n "," #'tabulated-list-sort
      :n "{" #'tabulated-list-narrow-current-column
      :n "}" #'tabulated-list-widen-current-column
      )

(map! :map tabulated-list-mode-map
      :n "w" #'tabulated-list-next-column
      :n "b" #'tabulated-list-previous-column
      )
;;-- end flycheck

;;-- tree-sitter

(evil-define-key 'normal 'tree-sitter-mode
  "[g" +tree-sitter-goto-previous-map
  "]g" +tree-sitter-goto-next-map
  )
(map! :map tree-sitter-mode-map
      (:prefix "i"
               :vo "A" (+tree-sitter-get-textobj '("parameter.inner" "call.inner"))
               :vo "f" (+tree-sitter-get-textobj "function.inner")
               :vo "F" (+tree-sitter-get-textobj "call.inner")
               :vo "C" (+tree-sitter-get-textobj "class.inner")
               :vo "v" (+tree-sitter-get-textobj "conditional.inner")
               :vo "l" (+tree-sitter-get-textobj "loop.inner")
        )
      (:prefix "o"
               :vo "A" (+tree-sitter-get-textobj '("parameter.outer" "call.outer"))
               :vo "f" (+tree-sitter-get-textobj "function.outer")
               :vo "F" (+tree-sitter-get-textobj "call.outer")
               :vo "C" (+tree-sitter-get-textobj "class.outer")
               :vo "c" (+tree-sitter-get-textobj "comment.outer")
               :vo "v" (+tree-sitter-get-textobj "conditional.outer")
               :vo "l" (+tree-sitter-get-textobj "loop.outer")
               )
      (:prefix "[g"
            :n "a" (+tree-sitter-goto-textobj "parameter.outer" t)
            :n "f" (+tree-sitter-goto-textobj "function.outer" t)
            :n "F" (+tree-sitter-goto-textobj "call.outer" t)
            :n "C" (+tree-sitter-goto-textobj "class.outer" t)
            :n "c" (+tree-sitter-goto-textobj "comment.outer" t)
            :n "v" (+tree-sitter-goto-textobj "conditional.outer" t)
            :n "l" (+tree-sitter-goto-textobj "loop.outer" t)
            )
      (:prefix "]g"
            :n "a" (+tree-sitter-goto-textobj "parameter.outer")
            :n "f" (+tree-sitter-goto-textobj "function.outer")
            :n "F" (+tree-sitter-goto-textobj "call.outer")
            :n "C" (+tree-sitter-goto-textobj "class.outer")
            :n "c" (+tree-sitter-goto-textobj "comment.outer")
            :n "v" (+tree-sitter-goto-textobj "conditional.outer")
            :n "l" (+tree-sitter-goto-textobj "loop.outer")
      )
)
;;-- end tree-sitter

(setq lsp-mode-map           jg-lsp-mode-map
      lsp-command-map        jg-lsp-command-map
      lsp-signature-mode-map jg-lsp-signature-mode-map
      lsp-ui-imenu-mode-map  jg-lsp-ui-imenu-mode-map
      )

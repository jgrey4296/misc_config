;;; +bindings.el -*- lexical-binding: t; -*-

(setq lsp-mode-map (make-sparse-keymap)
      lsp-command-map (make-sparse-keymap)
      lsp-signature-mode-map (make-sparse-keymap)
      lsp-ui-imenu-mode-map (make-sparse-keymap)
      )
(evil-make-overriding-map lsp-mode-map)

(map! :leader
      :prefix "c"
      :desc "LSP"                                   "l"   lsp-command-map
      :desc "LSP Code actions"                      "a"   #'lsp-avy-lens
      :desc "LSP Rename"                            "R"   #'lsp-rename
      :desc "List errors"                           "x"   #'flycheck-list-errors
      :desc "jump to symbol in current workspace"   "j"   #'+jg-lsp-describe-workspace-symbol
      ;; :desc "Jump to symbol in any workspace"       "J"   #'lsp-ivy-global-workspace-symbol
      )

(map! :leader
      :after flycheck
      :prefix "c"
      :desc "Flycheck" "!" flycheck-command-map
      )

(map! :map lsp-mode-map
      :n "g r" #'lsp-rename
      ;; :n "s '" #'lsp-ui-imenu
      :n "c x" #'flycheck-list-errors
      :n "s j" #'+jg-lsp-describe-workspace-symbol
      )

(map! :map lsp-ui-imenu-mode-map
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

(map! :map lsp-browser-mode-map

      )

(map! :map lsp-ui-peek-mode-map
      "j"   #'lsp-ui-peek--select-next
      "k"   #'lsp-ui-peek--select-prev
      "C-k" #'lsp-ui-peek--select-prev-file
      "C-j" #'lsp-ui-peek--select-next-file
      )

;;-- lsp commands
(map! :map lsp-command-map
      (:prefix ("=" . "Formatting"))
      (:prefix ("F" . "Folders"))
      (:prefix ("r" . "Refactoring"))
      (:prefix ("a" . "Actions"))
      )

(map! :map lsp-command-map ;; Formatting
      :prefix ("=" . "Formatting")
      :desc "format buffer" "=" #'lsp-format-buffer
      :desc "format region" "r" #'lsp-format-region
)

(map! :map lsp-command-map ;; Folders
      :prefix ("F" . "Folders")
      :desc "add folder"          "a" #'lsp-workspace-folders-add
      :desc "un-blacklist folder" "b" #'lsp-workspace-blacklist-remove
      :desc "remove folder"       "r" #'lsp-workspace-folders-remove
      )

(map! :map lsp-command-map ;; refactoring
      :prefix ("r" . "Refactoring")
      :desc "organize imports"             "o" #'lsp-organize-imports
      :desc "rename"                       "r" #'lsp-rename
      )

(map! :map lsp-command-map ;; actions
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

;;; +bindings.el -*- lexical-binding: t; -*-



(setq lsp-mode-map (make-sparse-keymap)
      lsp-command-map (make-sparse-keymap)
      lsp-signature-mode-map (make-sparse-keymap)
      )

(map! :leader
      :prefix "c"
      :desc "LSP"                                   "l"   lsp-command-map
      :desc "LSP Code actions"                      "a"   #'lsp-avy-lens
      :desc "LSP Rename"                            "R"   #'lsp-rename
      :desc "List errors"                           "x"   #'flycheck-list-errors
      ;; :desc "Jump to symbol in current workspace"   "j"   #'lsp-ivy-workspace-symbol
      ;; :desc "Jump to symbol in any workspace"       "J"   #'lsp-ivy-global-workspace-symbol
      )

(map! :leader
      :after flycheck
      :prefix "c"
      :desc "Flycheck" "!" flycheck-command-map
      )

;;-- lsp
(map! :map lsp-mode-map
      :n "g r" #'lsp-rename
      )
(map! :map lsp-command-map
      (:prefix ("w" . "Workspaces"))
      (:prefix ("=" . "Formatting"))
      (:prefix ("F" . "Folders"))
      (:prefix ("T" . "Toggles"))
      (:prefix ("g" . "Goto"))
      (:prefix ("h" . "Help"))
      (:prefix ("r" . "Refactoring"))
      (:prefix ("a" . "Actions"))
      (:prefix ("G" . "Peek"))
      )

(map! :map lsp-command-map ;; Workspaces
      :prefix ("w" . "Workspaces")
      :desc "disconnect"       "D" #'lsp-disconnect
      :desc "describe session" "d" #'lsp-describe-session
      :desc "shutdown server"  "q" #'lsp-workspace-shutdown
      :desc "restart server"   "r" #'lsp-workspace-restart
      :desc "start server"     "s" #'lsp
      :desc "Select Server"    "/" #'+lsp/switch-client
      :desc "Debug" "?" #'+jg-ide-debug-lsp
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

(map! :map lsp-command-map ;; toggles
      :prefix ("T". "Toggles")
      :desc "modeline diagnostics"  "D" #'lsp-modeline-diagnostics-mode
      :desc "log io"                "L" #'lsp-toggle-trace-io
      :desc "sideline"              "S" #'lsp-ui-sideline-mode
      :desc "treemacs integration"  "T" #'lsp-treemacs-sync-mode
      :desc "modeline code actions" "a" #'lsp-modeline-code-actions-mode
      :desc "breadcrumb"            "b" #'lsp-headerline-breadcrumb-mode
      :desc "documentation popup"   "d" #'lsp-ui-doc-mode
      :desc "on type formatting"    "f" #'lsp-toggle-on-type-formatting
      :desc "highlighting"          "h" #'lsp-toggle-symbol-highlight
      :desc "lenses"                "l" #'lsp-lens-mode
      :desc "signature"             "s" #'lsp-toggle-signature-auto-activate
      )

(map! :map lsp-command-map ;; goto
      :prefix ("g" . "Goto")
      :desc "find symbol in workspace"     "a" #'xref-find-apropos
      :desc "find declarations"            "d" #'lsp-find-declaration
      :desc "show errors"                  "e" #'lsp-treemacs-errors-list
      :desc "find definitions"             "g" #'lsp-find-definition
      :desc "call hierarchy"               "h" #'lsp-treemacs-call-hierarchy
      :desc "find implementations"         "i" #'lsp-find-implementation
      :desc "find references"              "r" #'lsp-find-references
      :desc "find type definition"         "t" #'lsp-find-type-definition
      )

(map! :map lsp-command-map ;; help
      :prefix ("h" . "Help")
      :desc "glance symbol"                "g" #'lsp-ui-doc-glance
      :desc "describe symbol at point"     "h" #'lsp-describe-thing-at-point
      :desc "signature help"               "s" #'lsp-signature-activate
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

(map! :map lsp-command-map ;; peek
      :prefix ("G" . "Peek")
      :desc "peek definitions"      "g" #'lsp-ui-peek-find-definitions
      :desc "peek implementations"  "i" #'lsp-ui-peek-find-implementation
      :desc "peek references"       "r" #'lsp-ui-peek-find-references
      :desc "peek workspace symbol" "s" #'lsp-ui-peek-find-workspace-symbol
)

(map! :map lsp-browser-mode-map

      )

(map! :map lsp-ui-peek-mode-map
      "j"   #'lsp-ui-peek--select-next
      "k"   #'lsp-ui-peek--select-prev
      "C-k" #'lsp-ui-peek--select-prev-file
      "C-j" #'lsp-ui-peek--select-next-file)

;;-- end lsp

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

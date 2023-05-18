;;; +bindings.el -*- lexical-binding: t; -*-

(setq lsp-mode-map (make-sparse-keymap)
      lsp-command-map (make-sparse-keymap)
      lsp-signature-mode-map (make-sparse-keymap)
      )

(map! :leader
      :prefix "c"
      :desc "LSP Code actions"                      "a"   #'lsp-execute-code-action
      :desc "LSP Organize imports"                  "o"   #'lsp-organize-imports
      :desc "LSP Rename"                            "R"   #'lsp-rename
      :desc "LSP"                                   "l"   lsp-command-map
      )

;;-- lsp
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
      :prefix ("t". "Toggles")
      :desc "toggle modeline diagnostics"  "D" #'lsp-modeline-diagnostics-mode
      :desc "toggle log io"                "L" #'lsp-toggle-trace-io
      :desc "toggle sideline"              "S" #'lsp-ui-sideline-mode
      :desc "toggle treemacs integration"  "T" #'lsp-treemacs-sync-mode
      :desc "toggle modeline code actions" "a" #'lsp-modeline-code-actions-mode
      :desc "toggle breadcrumb"            "b" #'lsp-headerline-breadcrumb-mode
      :desc "toggle documentation popup"   "d" #'lsp-ui-doc-mode
      :desc "toggle on type formatting"    "f" #'lsp-toggle-on-type-formatting
      :desc "toggle highlighting"          "h" #'lsp-toggle-symbol-highlight
      :desc "toggle lenses"                "l" #'lsp-lens-mode
      :desc "toggle signature"             "s" #'lsp-toggle-signature-auto-activate
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

;;-- eglot

;;-- end eglot

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

;;-- tree-sitter
(evil-define-key '(visual operator) 'tree-sitter-mode
  "i" +tree-sitter-inner-text-objects-map
  "a" +tree-sitter-outer-text-objects-map
  )
(evil-define-key 'normal 'tree-sitter-mode
  "[g" +tree-sitter-goto-previous-map
  "]g" +tree-sitter-goto-next-map
  )
(map! (:map +tree-sitter-inner-text-objects-map
            "A" (+tree-sitter-get-textobj '("parameter.inner" "call.inner"))
            "f" (+tree-sitter-get-textobj "function.inner")
            "F" (+tree-sitter-get-textobj "call.inner")
            "C" (+tree-sitter-get-textobj "class.inner")
            "v" (+tree-sitter-get-textobj "conditional.inner")
            "l" (+tree-sitter-get-textobj "loop.inner"))
      (:map +tree-sitter-outer-text-objects-map
            "A" (+tree-sitter-get-textobj '("parameter.outer" "call.outer"))
            "f" (+tree-sitter-get-textobj "function.outer")
            "F" (+tree-sitter-get-textobj "call.outer")
            "C" (+tree-sitter-get-textobj "class.outer")
            "c" (+tree-sitter-get-textobj "comment.outer")
            "v" (+tree-sitter-get-textobj "conditional.outer")
            "l" (+tree-sitter-get-textobj "loop.outer")
            )

      (:map +tree-sitter-goto-previous-map
            "a" (+tree-sitter-goto-textobj "parameter.outer" t)
            "f" (+tree-sitter-goto-textobj "function.outer" t)
            "F" (+tree-sitter-goto-textobj "call.outer" t)
            "C" (+tree-sitter-goto-textobj "class.outer" t)
            "c" (+tree-sitter-goto-textobj "comment.outer" t)
            "v" (+tree-sitter-goto-textobj "conditional.outer" t)
            "l" (+tree-sitter-goto-textobj "loop.outer" t))
      (:map +tree-sitter-goto-next-map
            "a" (+tree-sitter-goto-textobj "parameter.outer")
            "f" (+tree-sitter-goto-textobj "function.outer")
            "F" (+tree-sitter-goto-textobj "call.outer")
            "C" (+tree-sitter-goto-textobj "class.outer")
            "c" (+tree-sitter-goto-textobj "comment.outer")
            "v" (+tree-sitter-goto-textobj "conditional.outer")
            "l" (+tree-sitter-goto-textobj "loop.outer")))

;;-- end tree-sitter

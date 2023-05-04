;;; +bindings.el -*- lexical-binding: t; -*-

(map! :leader
      :prefix "c"
      :desc "LSP Code actions"                      "a"   #'lsp-execute-code-action
      :desc "LSP Organize imports"                  "o"   #'lsp-organize-imports
      :desc "LSP Rename"                            "R"   #'lsp-rename
      :desc "LSP"                                   "l"   lsp-command-map
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

(map! :map lsp-browser-mode-map

      )

(map! :map lsp-ui-peek-mode-map
      "j"   #'lsp-ui-peek--select-next
      "k"   #'lsp-ui-peek--select-prev
      "C-k" #'lsp-ui-peek--select-prev-file
      "C-j" #'lsp-ui-peek--select-next-file)


(evil-define-key '(visual operator) 'tree-sitter-mode
  "i" +tree-sitter-inner-text-objects-map
  "a" +tree-sitter-outer-text-objects-map
  )
(evil-define-key 'normal 'tree-sitter-mode
  "[g" +tree-sitter-goto-previous-map
  "]g" +tree-sitter-goto-next-map
  )

;;-- tree-sitter text obs
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

;;-- end tree-sitter text obs

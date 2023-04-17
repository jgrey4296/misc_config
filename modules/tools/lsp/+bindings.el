;;; +bindings.el -*- lexical-binding: t; -*-

(map! :map lsp-browser-mode-map

      )

(map! :map lsp-ui-peek-mode-map
      "j"   #'lsp-ui-peek--select-next
      "k"   #'lsp-ui-peek--select-prev
      "C-k" #'lsp-ui-peek--select-prev-file
      "C-j" #'lsp-ui-peek--select-next-file)

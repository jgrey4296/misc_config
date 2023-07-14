;;; extra.el -*- lexical-binding: t; -*-


;;;###autoload
(defun +lua-generate-lsp-server-command ()
      ;; The absolute path to lua-language-server binary is necessary because
      ;; the bundled dependencies aren't found otherwise. The only reason this
      ;; is a function is to dynamically change when/if `+lua-lsp-dir' does
      (list (or (executable-find "lua-language-server")
                (doom-path +lua-lsp-dir
                           (cond (IS-MAC     "bin/macOS")
                                 (IS-LINUX   "bin/Linux")
                                 (IS-WINDOWS "bin/Windows"))
                           "lua-language-server"))
            "-E" "-e" "LANG=en"
            (doom-path +lua-lsp-dir "main.lua")))

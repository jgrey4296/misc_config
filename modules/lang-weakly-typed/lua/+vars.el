;;; +vars.el -*- lexical-binding: t; -*-

(defvar +lua-lsp-dir (concat doom-data-dir "lsp/lua-language-server/")
  "Absolute path to the directory of sumneko's lua-language-server.
This directory MUST contain the 'main.lua' file and be the in-source build of
lua-language-server.")


(speckler-add! company
                    '(lua-mode (:mode company-lua))
                    )

(speckler-add! lookup-handler
                    `(lua-mode
                      :documentation ,#'lua-search-documentation
                      )
                    `(fennel-mode
                      :definition ,#'fennel-find-definition
                      :documentation ,#'fennel-show-documentation
                      )
                    )

(speckler-add! auto-modes
                    '(lua
                      ("\\.lua\\'" . lua-mode)
                      )
                    )

(speckler-add! electric
                      '(lua-mode
                        :words ("else" "end")
                        )
                      )

(speckler-add! repl
                      '(lua-mode    :start +lua/open-repl)
                      '(fennel-mode :start fennel-repl)
                      )

(speckler-add! eglot
                    `(lua-mode ,(+lua-generate-lsp-server-command))
                    )

(speckler-add! babel
                    '(lua
                      (:name lua        :lib ob-lua)
                      )
                    )
(speckler-add! org-src
                    '(lua
                      ("lua" . lua)
                      )
                   )

;; (speckler-add! projects
;;                     `(love2d ,#'+lua-love-project-root :run ,#'+lua-love-build-command)
;;                     )

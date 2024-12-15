;;; +vars.el -*- lexical-binding: t; -*-

(defvar +lua-lsp-dir (concat doom-data-dir "lsp/lua-language-server/")
  "Absolute path to the directory of sumneko's lua-language-server.
This directory MUST contain the 'main.lua' file and be the in-source build of
lua-language-server.")


(spec-handling-add! company
                    '(lua-mode (:mode company-lua))
                    )

(spec-handling-add! lookup-handler
                    `(lua-mode
                      :documentation ,#'lua-search-documentation
                      )
                    `(fennel-mode
                      :definition ,#'fennel-find-definition
                      :documentation ,#'fennel-show-documentation
                      )
                    )

(spec-handling-add! auto-modes
                    '(lua
                      ("\\.lua\\'" . lua-mode)
                      )
                    )

(spec-handling-add! electric
                      '(lua-mode
                        :words ("else" "end")
                        )
                      )

(spec-handling-add! repl
                      '(lua-mode    :start +lua/open-repl)
                      '(fennel-mode :start fennel-repl)
                      )

(spec-handling-add! eglot
                    `(lua-mode ,(+lua-generate-lsp-server-command))
                    )

(spec-handling-add! babel
                    '(lua
                      (:name lua        :lib ob-lua)
                      )
                    )
(spec-handling-add! org-src
                    '(lua
                      ("lua" . lua)
                      )
                   )

;; (spec-handling-add! projects
;;                     `(love2d ,#'+lua-love-project-root :run ,#'+lua-love-build-command)
;;                     )

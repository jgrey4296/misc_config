;;; +vars.el -*- lexical-binding: t; -*-

(spec-handling-add! lookup-regular
                    '(lua-mode
                      ("Lua Docs" . "https://www.lua.org/docs.html")
                      ("Garrys Mod" . "https://wiki.facepunch.com/gmod/Beginner_Tutorial_Intro")
                      ("Lua Manual" . "https://www.lua.org/manual/5.4/")
                      )
                    )

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

(spec-handling-add! eval
                      `(lua-mode :start ,#'+lua/open-repl)
                      `(fennel-mode :start ,#'fennel-repl)
                      )

(spec-handling-add! eglot
                    `(lua-mode ,(+lua-generate-lsp-server-command))
                    )

;; (spec-handling-add! projects
;;                     `(love2d ,#'+lua-love-project-root :run ,#'+lua-love-build-command)
;;                     )

;;; +vars.el -*- lexical-binding: t; -*-

(spec-handling-add! company
                    '(nix-mode (:mode . company-nixos-options))
                    )
(spec-handling-add! lookup-handler
                    '(nix-mode
                     :documentation (+nix/lookup-option :async t)
                     )
                    )
(spec-handling-add! popup
                    '(nix
                      ("^\\*nixos-options-doc\\*$" :ttl 0 :quit t)
                      )
                    )
(spec-handling-add! tree-sit-lang
                    '(nix-mode . nix)
                    )
(spec-handling-add! auto-modes
                    '(nix
                      ("\\.nix\\'" . nix-mode)
                      )
                    )

(spec-handling-add! repl
                    '(nix-mode :start +nix/open-repl)
                    )

(configuration-layer/declare-layers '(
                                      better-defaults
                                      (org :packages (not helm-org))
                                      gtags
                                      cscope
                                      semantic

                                      (syntax-checking :variables syntax-checking-enable-tooltips nil)
                                      (shell :variables
                                             shell-default-height 30
                                             shell-default-position 'bottom)
)

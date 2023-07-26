;;; +vars.el -*- lexical-binding: t; -*-

(spec-handling-add! auto-modes
                    '(rest
                      ("\\.http\\'" . restclient-mode)
                      )
                    )

(spec-handling-add! popup
                    '(rest
                      ("^\\*HTTP Response" :size 0.4 :quit 'other)
                      )
                    )


(spec-handling-add! company
                    '(restclient-mode (:mode company-restclient))
                    )

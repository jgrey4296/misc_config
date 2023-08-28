;;; +vars.el -*- lexical-binding: t; -*-

;; +vars.el<8> -*- lexical-binding: t; -*-

(setq gdb-show-main t
      gdb-many-windows t)


(spec-handling-add! popup
                    '(realgud
                      ("^\\*\\(?:trepanjs:\\(?:g\\|zsh\\|bash\\)db\\|pdb \\)" :size 20 :select nil :quit nil)
                      )
                    )

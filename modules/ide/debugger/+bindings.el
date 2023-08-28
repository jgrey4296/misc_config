;;; +bindings.el -*- lexical-binding: t; -*-


  (map! :localleader
        :map +dap-running-session-mode-map
        "d" #'dap-hydra)

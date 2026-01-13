;;; +bindings.el -*- lexical-binding: t; -*-


(map! :map +dap-running-session-mode-map
      :localleader
      "d" #'dap-hydra)

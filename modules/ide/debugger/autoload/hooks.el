;; hooks.el -*- lexical-binding: t; -*-
(defun +debugger-dap-start-on-stack (session)
  (when (dap--session-running session)
    (+dap-running-session-mode 1))
  )

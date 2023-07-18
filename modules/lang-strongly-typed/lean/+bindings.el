;;; +bindings.el -*- lexical-binding: t; -*-

(map! :map lean-mode-map
      :localleader
      "g" #'lean-toggle-show-goal
      "n" #'lean-toggle-next-error
      (:prefix ("s" . "server")
               "r" #'lean-server-restart
               "s" #'lean-server-stop
               "v" #'lean-server-switch-version)
      (:prefix ("p" . "leanpkg")
               "t" #'lean-leanpkg-test
               "b" #'lean-leanpkg-build
               "c" #'lean-leanpkg-configure)
      "f" #'lean-fill-placeholder
      "h" #'lean-hole
      "m" #'lean-message-boxes-toggle
      "e" #'lean-execute)

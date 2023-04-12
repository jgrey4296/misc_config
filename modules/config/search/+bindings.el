;;; +bindings.el -*- lexical-binding: t; -*-


(map! :leader
      (:prefix "s"
       :desc "Fd File" "f" #'fd-dired
       )
      (:prefix "f"
       :desc "Fd File" "l" #'fd-name-dired
       )
      )

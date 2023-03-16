;;; +bindings.el -*- lexical-binding: t; -*-

(map! :leader
      (:prefix "t"
       :desc "Visual Hydra" "v" '+jg-hydra-ui-toggles/body)
      "T" '+jg-hydra-ui-toggles/body
      )

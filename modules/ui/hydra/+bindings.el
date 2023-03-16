;;; +bindings.el -*- lexical-binding: t; -*-

(map! :leader
      (:prefix "t"
       :desc "Visual Hydra" "v" '+jg-ui-toggle-hydra/body)
      "T" '+jg-ui-toggle-hydra/body
      )

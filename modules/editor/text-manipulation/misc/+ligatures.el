;;; +ligatures.el -*- lexical-binding: t; -*-


;;-- ligatures
;; TODO move to text
(setq +ligatures-extra-symbols
      '(;; org
        :name          "»"
        :src_block     "»"
        :src_block_end "«"
        :quote         "“"
        :quote_end     "”"
        ;; Functional
        :lambda        "λ"
        ;;:def           "ƒ"
        :composition   "∘"
        :map           "↦"
        ;; Types
        :null          "∅"
        :true          "𝕋"
        :false         "𝔽"
        ;; :int           "ℤ"
        ;; :float         "ℝ"
        ;; :str           "𝕊"
        ;; :bool          "𝔹"
        ;; :list          "𝕃"
        ;; Flow
        :not           "¬"
        :in            "∈"
        :not-in        "∉"
        :and           "∧"
        :or            "∨"
        :for           "∀"
        :some          "∃"
        :return        "⏎"
        :yield         "⤶ "
        ;; Other
        :union         "⋃"
        :intersect     "∩"
        :diff          "∖"
        :tuple         "⨂ "
        :pipe          "|"
        :dot           "•")
      )

;;-- end ligatures

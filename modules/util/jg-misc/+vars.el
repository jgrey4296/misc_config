;;; util/jg-misc/+vars.el -*- lexical-binding: t; -*-

(setq-default shell-default-shell 'shell
              shell-protect-eshell-prompt 0
              shell-enable-smart-eshell t
              hl-paren-colors '("color-16" "color-16" "color-16" "color-16")
              hl-paren-background-colors '("Springgreen3" "color-26" "color-91" "IndianRed3")
              undo-tree-visualizer-diff t
              undo-tree-auto-save-history t
              undo-tree-enable-undo-in-region t
              ;; Increase undo-limits by a factor of ten to avoid emacs prematurely
              ;; truncating the undo history and corrupting the tree. See
              ;; https://github.com/syl20bnr/spacemacs/issues/12110
              undo-limit 800000
              undo-strong-limit 12000000
              undo-outer-limit 120000000
              undo-tree-history-directory-alist `(("." . ,(concat doom-cache-dir "undo-tree-hist/")))

              jg-misc-popup-persist-default '(:side bottom
                                              :height 0.3
                                              :quit t
                                              :select nil
                                              :modeline t
                                              :ttl nil)

              jg-misc-ivy-predicate-patterns (rx (or "*helpful"
                                                     "*Ibuffer"
                                                     "*helm-"
                                                     "doom"
                                                     "*dired-log"
                                                     "magit"
                                                     "*Free Keys"
                                                     )
                                              )
              )

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

(defvar +default-minibuffer-maps
  (append '(minibuffer-local-map
            minibuffer-local-ns-map
            minibuffer-local-completion-map
            minibuffer-local-must-match-map
            minibuffer-local-isearch-map
            read-expression-map)
          (cond ((featurep! :completion ivy)
                 '(ivy-minibuffer-map
                   ivy-switch-buffer-map))
                ((featurep! :completion helm)
                 '(helm-map
                   helm-rg-map
                   helm-read-file-map))))
  "A list of all the keymaps used for the minibuffer.")

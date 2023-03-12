;;; +vars.el -*- lexical-binding: t; -*-


(after! smartparens-markdown
  (sp-with-modes '(markdown-mode gfm-mode)
    (sp-local-pair "```" "```" :post-handlers '(:add ("||\n[i]" "RET")))

    ;; The original rules for smartparens had an odd quirk: inserting two
    ;; asterixex would replace nearby quotes with asterixes. These two rules
    ;; set out to fix this.
    (sp-local-pair "**" nil :actions :rem)
    (sp-local-pair "*" "*"
                   :actions '(insert skip)
                   :unless '(:rem sp-point-at-bol-p)
                   ;; * then SPC will delete the second asterix and assume
                   ;; you wanted a bullet point. * followed by another *
                   ;; will produce an extra, assuming you wanted **|**.
                   :post-handlers '(("[d1]" "SPC") ("|*" "*"))))

  ;; This keybind allows * to skip over **.
  (map! :map markdown-mode-map
        :ig "*" (general-predicate-dispatch nil
                  (looking-at-p "\\*\\* *")
                  (cmd! (forward-char 2)))))

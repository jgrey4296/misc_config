;; Sub-Hydras for the main `jg-ui-toggle-hydra'

(defhydra +jg-hydra-visuals (:color teal)
  (format "%s\n"
          (+jg-hydra-format-columns
           '(Visuals
             "_g_ Evil goggles"
             "_H_ Highlight Symbols"
             "_h_ Hl-line"
             "_I_ Ignore Invisible"
             "_p_ Highlight Parens"
             "_r_ Rainbow Mode"
             "_s_ Prettify Symbols Mode"
             )
           '("T/F"
             "%(+jg-hydra-doc evil-goggles-mode)"
             "%(+jg-hydra-doc auto-highlight-symbol-mode)"
             "%(+jg-hydra-doc hl-line-mode)"
             "%(+jg-hydra-doc line-move-ignore-invisible)"
             "%(+jg-hydra-doc global-highlight-parentheses-mode)"
             "%(+jg-hydra-doc rainbow-mode)"
             "%(+jg-hydra-doc prettify-symbols-mode)"
             )
           )
          )
  ("g" #'evil-goggles-mode                        nil :exit nil)
  ("H" #'auto-highlight-symbol-mode               nil :exit nil)
  ("h" #'global-hl-line-mode                      nil :exit nil)
  ("I" #'+jg-ui-toggle-line-move-ignore-invisible nil :exit nil)
  ("p" #'global-highlight-parentheses-mode        nil :exit nil)
  ("r" #'rainbow-mode                             nil :exit nil)
  ("s" #'prettify-symbols-mode                    nil :exit nil)
  ("q" +jg-hydra-pop "exit" :exit t)
  ("V" nil)
  ("T" nil)
  )

(defhydra +jg-hydra-guides (:color blue)
  (format "%s\n"
          (+jg-hydra-format-columns
           '(Guides
             "_C_ Fill Column Indicator"
             "_i_ Indent guides"
             "_n_ Line Numbers"
             "_R_ Ruler"
             "_w_ Whitespace"
             "_g_ Grammar"
             )
           '("T/F"
             "%(+jg-hydra-doc display-fill-column-indicator)"
             "%(+jg-hydra-doc highlight-indent-guides-mode)"
             "%(+jg-hydra-doc display-line-numbers)"
             "%(+jg-hydra-doc ruler-mode)"
             "%(+jg-hydra-doc whitespace-mode)"
             "%(+jg-hydra-doc (or flyspell-mode writegood-mode))"
             )
           )
          )
  ("C" #'display-fill-column-indicator-mode nil :exit nil)
  ("i" #'highlight-indent-guides-mode       nil :exit nil)
  ("n" #'+jg-ui-toggle-line-numbers         nil :exit nil)
  ("R" #'ruler-mode                         nil :exit nil)
  ("w" #'whitespace-mode                    nil :exit nil)
  ("g" (progn (flyspell-mode 'toggle) (writegood-mode (unless flyspell-mode -1))) nil :exit nil)
  ("q" +jg-hydra-pop "exit" :exit t)
  ("G" nil)
  )

(defhydra +jg-hydra-wrap (:color green)
  (format "%s\n"
          (+jg-hydra-format-columns
           '(Wrapping
             "_f_ Auto-fill"
             "_l_ Soft line wrapping"
             "_t_ Line Truncate"
             "_W_ Word-wrap mode"
             )
           '("T/F"
            "%(+jg-hydra-doc auto-fill-function)"
            "%(+jg-hydra-doc visual-line-mode)"
            "%(+jg-hydra-doc truncate-lines)"
            "%(+jg-hydra-doc +word-wrap-mode)"
            )))
  ("f" #'auto-fill-mode        nil :exit nil)
  ("l" #'visual-line-mode      nil :exit nil)
  ("t" #'toggle-truncate-lines nil :exit nil)
  ("W" #'+word-wrap-mode       nil :exit nil)
  ("q" +jg-hydra-pop "exit" :exit t)
  )

(defhydra +jg-hydra-nav (:color yellow)
  (format "%s\n"
          (+jg-hydra-format-columns
           '(Navigation
             "_a_  Auto-hide"
             "_b_  Auto-Balance"
             "_c_  Center Cursor"
             "_o_  Org-links"
             "_v_  Evil Visual Marks"
             "_T_  Neotree"
             "_M_  Minimap"
             "_F_  Frame fullscreen"
             )
           '("T/F"
             "%(+jg-hydra-doc global-autohide-minor-mode)"
             "%(+jg-hydra-doc evil-auto-balance-windows)"
             "%(+jg-hydra-doc global-centered-cursor-mode)"
             "%(+jg-hydra-doc org-link-descriptive)"
             "%(+jg-hydra-doc evil-visual-mark-mode)"
             )
           ))
  ("N" t)
  ("a" #'global-autohide-minor-mode  nil :exit nil)
  ("b" (setq evil-auto-balance-windows (not evil-auto-balance-windows))  nil :exit nil)
  ("c" #'global-centered-cursor-mode nil :exit nil)
  ("M" #'minimap-mode                nil :exit nil)
  ("o" #'org-toggle-link-display     nil :exit nil)
  ("T" #'neotree-toggle              nil :exit nil)
  ("v" #'evil-visual-mark-mode       nil :exit nil)
  ("F" #'toggle-frame-fullscreen     nil :exit nil)
  ("q" +jg-hydra-pop "exit" :exit t)
  )

;; Sub-Hydras for the main `jg-ui-toggle-hydra'

(defhydra +jg-hydra-visuals (:color teal)
  (format "%s\n"
          (+jg-hydra-format-columns
           '("|Visuals"
             "Evil _g_oggles"
             "Highlight _S_ymbols"
             "hl-line"
             "ignore _i_nvisible"
             )
           '(blank
             "%-10(+jg-hydra-doc evil-goggles-mode)"
             "%-10(+jg-hydra-doc auto-highlight-symbol-mode)"
             "%-10(+jg-hydra-doc hl-line-mode)"
             "%-10(+jg-hydra-doc line-move-ignore-invisible)"
             )
           '(blank
             "Highlight _w_rappers"
             "Rainbow _c_olours"
             "pretty Symbols"
             "quick_s_cope"
             "high_l_ight changes"
             )
           '(blank
             "%-10(+jg-hydra-doc highlight-parentheses-mode)"
             "%-10(+jg-hydra-doc rainbow-mode)"
             "%-10(+jg-hydra-doc prettify-symbols-mode)"
             "%(+jg-hydra-doc evil-quickscope-always-mode)/%-4(+jg-hydra-doc evil-quickscope-mode)"
             "%-10(+jg-hydra-doc highlight-changes-visible-mode)"
             )
           )
          )
  ("g" #'evil-goggles-mode                        nil :exit nil)
  ("S" #'auto-highlight-symbol-mode               nil :exit nil)
  ("h" #'hl-line-mode                             nil :exit nil)
  ("i" #'+jg-ui-toggle-line-move-ignore-invisible nil :exit nil)
  ("w" #'highlight-parentheses-mode               nil :exit nil)
  ("c" #'rainbow-mode                             nil :exit nil)
  ("p" #'prettify-symbols-mode                    nil :exit nil)
  ("l" #'highlight-changes-visible-mode           nil :exit nil)
  ("s" (progn (evil-quickscope-always-mode 'toggle)
              (evil-quickscope-mode (if evil-quickscope-always-mode -1 1)))
   nil :exit nil)
  ("q" +jg-hydra-pop "exit" :exit t)
  )

(defhydra +jg-hydra-guides (:color blue)
  (format "%s\n"
          (+jg-hydra-format-columns
           '(Guides
             "_C_ Fill Column Indicator"
             "_i_ Indent guides"
             "_n_ Line Numbers"
             "_R_ Ruler"
             )
           '(blank
             "%-10(+jg-hydra-doc display-fill-column-indicator)"
             "%-10(+jg-hydra-doc highlight-indent-guides-mode)"
             "%-10(+jg-hydra-doc display-line-numbers)"
             "%-10(+jg-hydra-doc ruler-mode)"
             )
           '(blank
             "_w_ Whitespace"
             "_g_ Grammar"
             )
           '(blank
             "%-10(+jg-hydra-doc whitespace-mode)"
             "%-10(+jg-hydra-doc (or flyspell-mode writegood-mode))"
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
           '(blank
            "%-10(+jg-hydra-doc auto-fill-function)"
            "%-10(+jg-hydra-doc visual-line-mode)"
            "%-10(+jg-hydra-doc truncate-lines)"
            "%-10(+jg-hydra-doc +word-wrap-mode)"
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
             )
           '(blank
             "%-10(+jg-hydra-doc global-autohide-minor-mode)"
             "%-10(+jg-hydra-doc evil-auto-balance-windows)"
             "%-10(+jg-hydra-doc centered-cursor-mode)"
             "%-10(+jg-hydra-doc org-link-descriptive)"
             "%-10(+jg-hydra-doc evil-visual-mark-mode)"
             )
           '(blank
             "_T_  Neotree"
             "_M_  Minimap"
             "_F_  Frame fullscreen"
             )
           ))
  ("N" t)
  ("a" #'global-autohide-minor-mode  nil :exit nil)
  ("b" (setq evil-auto-balance-windows (not evil-auto-balance-windows))  nil :exit nil)
  ("c" #'centered-cursor-mode        nil :exit nil)
  ("M" #'minimap-mode                nil :exit nil)
  ("o" #'org-toggle-link-display     nil :exit nil)
  ("T" #'neotree-toggle              nil :exit nil)
  ("v" #'evil-visual-mark-mode       nil :exit nil)
  ("F" #'toggle-frame-fullscreen     nil :exit nil)
  ("q" +jg-hydra-pop "exit" :exit t)
  )

;; Sub-Hydras for the main `jg-ui-toggle-hydra'

(defhydra +jg-hydra-visuals (:color teal)
  (format "%s\n"
          (+jg-hydra-format-columns '(
"Visuals
-------------------------
_g_ Evil goggles          %(+jg-hydra-doc evil-goggles-mode)
_H_ Highlight Symbols     %(+jg-hydra-doc auto-highlight-symbol-mode)
_h_ Hl-line               %(+jg-hydra-doc hl-line-mode)
_I_ Ignore Invisible      %(+jg-hydra-doc line-move-ignore-invisible)
_p_ Highlight Parens      %(+jg-hydra-doc global-highlight-parentheses-mode)
_r_ Rainbow Mode          %(+jg-hydra-doc rainbow-mode)
_s_ Prettify Symbols Mode %(+jg-hydra-doc prettify-symbols-mode)"

""
)))
  ("g" #'evil-goggles-mode                        nil :exit nil)
  ("H" #'auto-highlight-symbol-mode               nil :exit nil)
  ("h" #'global-hl-line-mode                      nil :exit nil)
  ("I" #'+jg-ui-toggle-line-move-ignore-invisible nil :exit nil)
  ("p" #'global-highlight-parentheses-mode        nil :exit nil)
  ("r" #'rainbow-mode                             nil :exit nil)
  ("s" #'prettify-symbols-mode                    nil :exit nil)
  ("q" +jg-hydra-pop "exit" :exit t)
  )

(defhydra +jg-hydra-guides (:color blue)
  (format "%s\n"
          (+jg-hydra-format-columns '(
"Guides
--------------------------
_C_ Fill Column Indicator  %(+jg-hydra-doc display-fill-column-indicator)
_i_ Indent guides          %(+jg-hydra-doc highlight-indent-guides-mode)
_n_ Line Numbers           %(+jg-hydra-doc display-line-numbers)
_R_ Ruler                  %(+jg-hydra-doc ruler-mode)
_w_ Whitespace             %(+jg-hydra-doc whitespace-mode)
_g_ Grammar                %(+jg-hydra-doc (or flyspell-mode writegood-mode))"

""
)))
  ("C" #'display-fill-column-indicator-mode nil :exit nil)
  ("i" #'highlight-indent-guides-mode       nil :exit nil)
  ("n" #'+jg-ui-toggle-line-numbers         nil :exit nil)
  ("R" #'ruler-mode                         nil :exit nil)
  ("w" #'whitespace-mode                    nil :exit nil)
  ("g" (progn (flyspell-mode 'toggle) (writegood-mode (unless flyspell-mode -1))) nil :exit nil)
  ("q" +jg-hydra-pop "exit" :exit t)
  )

(defhydra +jg-hydra-wrap (:color green)
  (format "%s\n"
          (+jg-hydra-format-columns '(
"Wrapping
-------------------------
_f_ Auto-fill             %(+jg-hydra-doc auto-fill-function)
_l_ Soft line wrapping    %(+jg-hydra-doc visual-line-mode)
_t_ Line Truncate         %(+jg-hydra-doc truncate-lines)
_W_ Word-wrap mode        %(+jg-hydra-doc +word-wrap-mode)"

""
)))
  ("f" #'auto-fill-mode        nil :exit nil)
  ("l" #'visual-line-mode      nil :exit nil)
  ("t" #'toggle-truncate-lines nil :exit nil)
  ("W" #'+word-wrap-mode       nil :exit nil)
  ("q" +jg-hydra-pop "exit" :exit t)
  )

(defhydra +jg-hydra-nav(:color yellow)
  (format "%s\n"
          (+jg-hydra-format-columns '(
"Misc                   |
------------------------
_a_  Auto-hide           %(+jg-hydra-doc global-autohide-minor-mode)
_c_  Center Cursor       %(+jg-hydra-doc global-centered-cursor-mode)
_o_  Org-links           %(+jg-hydra-doc org-link-descriptive)
_v_  Evil Visual Marks   %(+jg-hydra-doc evil-visual-mark-mode)
_T_  Neotree             *
_M_  Minimap             *
_F_  Frame fullscreen    *"

""
)))
  ("a" #'global-autohide-minor-mode  nil :exit nil)
  ("c" #'global-centered-cursor-mode nil :exit nil)
  ("M" #'minimap-mode                nil :exit nil)
  ("o" #'org-toggle-link-display     nil :exit nil)
  ("T" #'neotree-toggle              nil :exit nil)
  ("v" #'evil-visual-mark-mode       nil :exit nil)
  ("F" #'toggle-frame-fullscreen     nil :exit nil)
  ("q" +jg-hydra-pop "exit" :exit t)
  )

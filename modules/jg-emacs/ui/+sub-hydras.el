;; Sub-Hydras for the main `jg-ui-toggle-hydra'

(defhydra +jg-ui-visuals-hydra (:color teal)
  (format "%s\n"
          (+jg-text-format-hydra-columns '(
"Visuals
-------------------------
_g_ Evil goggles          %(+jghdoc evil-goggles-mode)
_H_ Highlight Symbols     %(+jghdoc auto-highlight-symbol-mode)
_h_ Hl-line               %(+jghdoc hl-line-mode)
_I_ Ignore Invisible      %(+jghdoc line-move-ignore-invisible)
_p_ Highlight Parens      %(+jghdoc global-highlight-parentheses-mode)
_r_ Rainbow Mode          %(+jghdoc rainbow-mode)
_s_ Prettify Symbols Mode %(+jghdoc prettify-symbols-mode)"

""
)))
  ("g" #'evil-goggles-mode                        nil :exit nil)
  ("H" #'auto-highlight-symbol-mode               nil :exit nil)
  ("h" #'global-hl-line-mode                      nil :exit nil)
  ("I" #'+jg-ui-toggle-line-move-ignore-invisible nil :exit nil)
  ("p" #'global-highlight-parentheses-mode        nil :exit nil)
  ("r" #'rainbow-mode                             nil :exit nil)
  ("s" #'prettify-symbols-mode                    nil :exit nil)
  ("q" +jgh-pop "exit" :exit t)
  )

(defhydra +jg-ui-guides-hydra (:color blue)
  (format "%s\n"
          (+jg-text-format-hydra-columns '(
"Guides
--------------------------
_C_ Fill Column Indicator  %(+jghdoc display-fill-column-indicator)
_i_ Indent guides          %(+jghdoc highlight-indent-guides-mode)
_n_ Line Numbers           %(+jghdoc display-line-numbers)
_R_ Ruler                  %(+jghdoc ruler-mode)
_w_ Whitespace             %(+jghdoc whitespace-mode)
_g_ Grammar                %(+jghdoc (or flyspell-mode writegood-mode))"

""
)))
  ("C" #'display-fill-column-indicator-mode nil :exit nil)
  ("i" #'highlight-indent-guides-mode       nil :exit nil)
  ("n" #'+jg-ui-toggle-line-numbers         nil :exit nil)
  ("R" #'ruler-mode                         nil :exit nil)
  ("w" #'whitespace-mode                    nil :exit nil)
  ("g" (progn (flyspell-mode 'toggle) (writegood-mode (unless flyspell-mode -1))) nil :exit nil)
  ("q" +jgh-pop "exit" :exit t)
  )

(defhydra +jg-ui-wrap-hydra (:color green)
  (format "%s\n"
          (+jg-text-format-hydra-columns '(
"Wrapping
-------------------------
_f_ Auto-fill             %(+jghdoc auto-fill-function)
_l_ Soft line wrapping    %(+jghdoc visual-line-mode)
_t_ Line Truncate         %(+jghdoc truncate-lines)
_W_ Word-wrap mode        %(+jghdoc +word-wrap-mode)"

""
)))
  ("f" #'auto-fill-mode        nil :exit nil)
  ("l" #'visual-line-mode      nil :exit nil)
  ("t" #'toggle-truncate-lines nil :exit nil)
  ("W" #'+word-wrap-mode       nil :exit nil)
  ("q" +jgh-pop "exit" :exit t)
  )

(defhydra +jg-ui-nav-hydra (:color yellow)
  (format "%s\n"
          (+jg-text-format-hydra-columns '(
"Misc                   |
------------------------
_a_  Auto-hide           %(+jghdoc global-autohide-minor-mode)
_c_  Center Cursor       %(+jghdoc global-centered-cursor-mode)
_o_  Org-links           %(+jghdoc org-link-descriptive)
_v_  Evil Visual Marks   %(+jghdoc evil-visual-mark-mode)
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
  ("q" +jgh-pop "exit" :exit t)
  )

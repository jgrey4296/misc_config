;;; +hydra.el -*- lexical-binding: t; -*-

(defun +jghdoc (var)
  (if var 1 0)
  )

;; Row padder:
;; "*                   ^^ *"
(defhydra +jg-ui-visual-toggle()
  (format "%s\n"
          (+jg-text-format-hydra-columns '(
"Visuals              |
----------------------
_p_ Highlight Parens   %(+jghdoc global-highlight-parentheses-mode)
_g_ Evil goggles       %(+jghdoc evil-goggles-mode)
_h_ Hl-line            %(+jghdoc hl-line-mode)
_H_ Highlight Symbols  %(+jghdoc auto-highlight-symbol-mode)
_I_ Ignore Invisible   %(+jghdoc line-move-ignore-invisible)
_F_ Frame fullscreen   *
*                   ^^ *"

"Guides                   |
--------------------------
_i_ Indent guides          %(+jghdoc highlight-indent-guides-mode)
_C_ Fill Column Indicator  %(+jghdoc display-fill-column-indicator)
_w_ Whitespace             %(+jghdoc whitespace-mode)
_s_ Prettify Symbols Mode  %(+jghdoc prettify-symbols-mode)
_R_ Ruler                  %(+jghdoc ruler-mode)
_n_ Line Numbers           %(+jghdoc display-line-numbers)
*                       ^^ *"

"Wrapping                |
-------------------------
_W_ Word-wrap mode        %(+jghdoc +word-wrap-mode)
_f_ Auto-fill             %(+jghdoc auto-fill-function)
_t_ Line Truncate         %(+jghdoc truncate-lines)
_l_ Soft line wrapping    %(+jghdoc visual-line-mode)
*                      ^^ *
*                      ^^ *
*                      ^^ *"

"Misc                   |
------------------------
_o_  Org-links           %(+jghdoc org-link-descriptive)
_v_  Evil Visual Marks   %(+jghdoc evil-visual-mark-mode)
_a_  Auto-hide           %(+jghdoc global-autohide-minor-mode)
_c_  Center Cursor       %(+jghdoc global-centered-cursor-mode)
_T_  Neotree             *
_M_  Minimap             *
*                     ^^ *"
)))

  ("a" #'global-autohide-minor-mode               nil)
  ("c" #'global-centered-cursor-mode              nil)
  ("C" #'display-fill-column-indicator-mode       nil)
  ("f" #'auto-fill-mode                           nil)
  ("F" #'toggle-frame-fullscreen                  nil)
  ("g" #'evil-goggles-mode                        nil)
  ("H" #'auto-highlight-symbol-mode               nil)
  ("h" #'global-hl-line-mode                      nil)
  ("I" #'+jg-ui-toggle-line-move-ignore-invisible nil)
  ("i" #'highlight-indent-guides-mode             nil)
  ("l" #'visual-line-mode                         nil)
  ("M" #'minimap-mode                             nil)
  ("n" #'+jg-ui-toggle-line-numbers               nil)
  ("o" #'org-toggle-link-display                  nil)
  ("p" #'global-highlight-parentheses-mode        nil)
  ("R" #'ruler-mode                               nil)
  ("s" #'prettify-symbols-mode                    nil)
  ("S" #'+neotree/open                            nil)
  ("T" #'neotree-toggle                           nil)
  ("t" #'toggle-truncate-lines                    nil)
  ("v" #'evil-visual-mark-mode                    nil)
  ("W" #'+word-wrap-mode                          nil)
  ("w" #'whitespace-mode                          nil)
  ("q" nil nil :exit t)
)


(after! jg-leader-bindings-loaded
  (map! :leader
        :prefix "t"
        :desc "Visual Hydra" "v" '+jg-ui-visual-toggle/body
        )
  )

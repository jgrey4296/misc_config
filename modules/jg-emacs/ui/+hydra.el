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
_F_ Frame fullscreen   *
_g_ Evil goggles       %(+jghdoc evil-goggles-mode)
_H_ Highlight Symbols  %(+jghdoc auto-highlight-symbol-mode)
_h_ Hl-line            %(+jghdoc hl-line-mode)
_I_ Ignore Invisible   %(+jghdoc line-move-ignore-invisible)
_p_ Highlight Parens   %(+jghdoc global-highlight-parentheses-mode)
*                   ^^ *"

"Guides                   |
--------------------------
_C_ Fill Column Indicator  %(+jghdoc display-fill-column-indicator)
_i_ Indent guides          %(+jghdoc highlight-indent-guides-mode)
_n_ Line Numbers           %(+jghdoc display-line-numbers)
_R_ Ruler                  %(+jghdoc ruler-mode)
_s_ Prettify Symbols Mode  %(+jghdoc prettify-symbols-mode)
_w_ Whitespace             %(+jghdoc whitespace-mode)
_G_ Writing                %(+jghdoc (or flyspell-mode writegood-mode))"

"Wrapping                |
-------------------------
_f_ Auto-fill             %(+jghdoc auto-fill-function)
_l_ Soft line wrapping    %(+jghdoc visual-line-mode)
_t_ Line Truncate         %(+jghdoc truncate-lines)
_W_ Word-wrap mode        %(+jghdoc +word-wrap-mode)
*                      ^^ *
*                      ^^ *
*                      ^^ *"

"Misc                   |
------------------------
_a_  Auto-hide           %(+jghdoc global-autohide-minor-mode)
_c_  Center Cursor       %(+jghdoc global-centered-cursor-mode)
_M_  Minimap             *
_o_  Org-links           %(+jghdoc org-link-descriptive)
_T_  Neotree             *
_v_  Evil Visual Marks   %(+jghdoc evil-visual-mark-mode)
*                     ^^ *"
"Spare                  |
------------------------
*                     ^^ *
*                     ^^ *
*                     ^^ *
*                     ^^ *
*                     ^^ *
*                     ^^ *
*                     ^^ *"
)))

  ("a" #'global-autohide-minor-mode               nil)
  ("c" #'global-centered-cursor-mode              nil)
  ("C" #'display-fill-column-indicator-mode       nil)
  ("f" #'auto-fill-mode                           nil)
  ("F" #'toggle-frame-fullscreen                  nil)
  ("g" #'evil-goggles-mode                        nil)
  ("G" (progn (flyspell-mode 'toggle) (writegood-mode (unless flyspell-mode -1))) nil)
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

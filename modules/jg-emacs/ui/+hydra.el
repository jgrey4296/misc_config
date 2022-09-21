;;; +hydra.el -*- lexical-binding: t; -*-

(defun +jghdoc (var)
  (if var 1 0)
  )

(defhydra +jg-ui-visual-toggle()
  "
^Visuals                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^   Guides                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^   Wrapping                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  Misc
-------------------------^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^------------------------------^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^---------------------------^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^-------------------------^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
_p_   Highlight Parens   %(+jghdoc global-highlight-parentheses-mode)   _i_  Indent guides          %(+jghdoc highlight-indent-guides-mode) ^^^^^^  _W_   Word-wrap mode     %(+jghdoc +word-wrap-mode) ^^^ _o_  Org-links          %(+jghdoc org-link-descriptive)
_g_   Evil goggles       %(+jghdoc evil-goggles-mode) ^^^^^^^^^^^^^^^^  _C_  Fill Column Indicator  %(+jghdoc display-fill-column-indicator) ^^^^^  _f_   Auto-fill          %(+jghdoc auto-fill-function)  _v_  Evil Visual Marks  %(+jghdoc evil-visual-mark-mode)
_h_   Hl-line            %(+jghdoc hl-line-mode) ^^^^^^^^^^^^^^^^^^^^^  _w_  Whitespace             %(+jghdoc whitespace-mode) ^^^^^^^^^^^^^^^^^^^  _t_   Line Truncate      %(+jghdoc truncate-lines) ^^^^ _a_  Auto-hide          %(+jghdoc jg-fold-auto-hide-toggle)
_H_   Highlight Symbols  %(+jghdoc auto-highlight-symbol-mode) ^^^^^^^  _s_  Prettify Symbols Mode  %(+jghdoc prettify-symbols-mode) ^^^^^^^^^^^^^  _l_   Soft line wrapping %(+jghdoc visual-line-mode) ^^ _c_  Center Cursor      %(+jghdoc centered-cursor-mode)
_I_   Ignore Invisible   %(+jghdoc line-move-ignore-invisible) ^^^^^^^  _R_  Ruler                  %(+jghdoc ruler-mode) ^^^^^^^^^^^^^^^^^^^^^^^   ^                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ _T_  Neotree
_F_   Frame fullscreen     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  _n_  Line Numbers           %(+jghdoc display-line-numbers) ^^^^^^^^^^^^^   ^                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ _M_  Minimap
"

  ("a" #'+jg-fold-toggle-auto-hide nil)
  ("c" #'centered-cursor-mode nil)
  ("C" #'display-fill-column-indicator-mode nil)
  ("f" #'auto-fill-mode nil)
  ("F" #'toggle-frame-fullscreen nil)
  ("g" #'evil-goggles-mode nil)
  ("H" #'auto-highlight-symbol-mode nil)
  ("h" #'global-hl-line-mode nil)
  ("I" #'+jg-ui-toggle-line-move-ignore-invisible nil)
  ("i" #'highlight-indent-guides-mode nil)
  ("l" #'visual-line-mode nil)
  ("M" #'minimap-mode nil)
  ("n" #'+jg-ui-toggle-line-numbers nil)
  ("o" #'org-toggle-link-display nil)
  ("p" #'global-highlight-parentheses-mode nil)
  ("R" #'ruler-mode nil)
  ("s" #'prettify-symbols-mode nil)
  ("T" #'neotree-toggle nil)
  ("t" #'toggle-truncate-lines nil)
  ("v" #'evil-visual-mark-mode nil)
  ("W" #'+word-wrap-mode nil)
  ("w" #'whitespace-mode nil)
  ("q" nil "quit")
  )

(after! jg-leader-bindings-loaded
  (map! :leader
        :prefix "t"
        :desc "Visual Hydra" "v" #'+jg-ui-visual-toggle/body
        )
  )

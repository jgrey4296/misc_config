;;; +hydra.el -*- lexical-binding: t; -*-

(require 'hydra)

(defhydra tag-clean ()
  "
               | Commands   ^^|
               |------------^^|------------^^|
               | [_q_] Quit   | [_!_] Split  |
               | [_f_] Filter | [_p_] Prev   |
               | [_s_] Sub    | [_l_] Leave  |
               "
  ("q" nil :exit t)
  ("f" #'tag-clean/mark-to-filter nil)
  ("s" #'tag-clean/mark-to-sub nil)
  ("p" #'tag-clean/previous nil)
  ("l" #'tag-clean/leave nil)
  ("!" #'+jg-tag-org-split-on-headings nil :exit t)
  )
(map! :after tag-clean-minor-mode
      :map tag-clean-minor-mode-map
      "." #'tag-clean/body
      )

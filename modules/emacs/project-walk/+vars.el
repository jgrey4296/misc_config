;;; +vars.el -*- lexical-binding: t; -*-

(setq jg-project-walk-popup-rules
      '(("^\\*Project-Walk\\*" :side left :ttl nil :quit t :select nil))
      )
(after! jg-popup-init
  (+jg-popup-add-rules 'proj-walk jg-project-walk-popup-rules)
  (+jg-popup-activate-rules)
  )

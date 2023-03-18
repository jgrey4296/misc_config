;;; +vars.el -*- lexical-binding: t; -*-

(defvar jg-ibuffer-filters (make-hash-table) "Hashtable of hashtables for defining ibuffer filters")
(defvar jg-ibuffer-filter-groups (make-hash-table) "Hashtable of hashtables for defining ibuffer filter groups")
(defvar jg-ibuffer-used-filter-names '())
(defvar jg-ibuffer-used-group-names '())

(setq ibuffer-show-empty-filter-groups nil
      ibuffer-filter-group-name-face '(:inherit (success bold))
      )

;;-- popup
(setq jg-ibuffer-popup-rules
      '(
        ("^\*Ibuffer\*$"         :side right  :ttl 5 :width  0.5 :quit nil :select t :priority 50)
        ))
(after! jg-popup-init
  (+jg-ui-popup-add-rules 'ibuffer jg-ibuffer-popup-rules)
  )
;;-- end popup

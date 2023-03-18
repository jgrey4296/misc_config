;;; +vars.el -*- lexical-binding: t; -*-

(defvar jg-ibuffer-filters (make-hash-table) "Hashtable of hashtables for defining ibuffer filters")
(defvar jg-ibuffer-filter-groups (make-hash-table) "Hashtable of hashtables for defining ibuffer filter groups")
(defvar jg-ibuffer-used-filter-names '())
(defvar jg-ibuffer-used-group-names '())

(setq ibuffer-show-empty-filter-groups nil
      ibuffer-filter-group-name-face '(:inherit (success bold))
      ibuffer-formats `(
                        ;; Normal
                        (mark modified read-only locked
                              " " (name 18 18 :left :elide)
                              " " (size 10 10 :right)
                              " " (mode 16 16 :left :elide)
                              " " project-relative-file)
                        ;; VC Status
                        (mark modified read-only locked
                              " " (name 18 18 :left :elide)
                              " " (size 10 10 :right)
                              " " vc-status
                              )
                        ;; Project
                        (mark " " (name 18 18 :left :elide)
                              " " (project-name 10 10 :left)
                              " " project-relative-file
                              )
                        )

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

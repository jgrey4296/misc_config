;;; +vars.el -*- lexical-binding: t; -*-


(setq ibuffer-show-empty-filter-groups nil
      ibuffer-filter-group-name-face '(:inherit (success bold))
      ibuffer-formats `(
                        (mark modified read-only locked " "
                              (name 18 18 :left :elide)
                              " "
                              (size 9 -1 :right)
                              " "
                              (mode 16 16 :left :elide)
                              " " filename-and-process)
                        ;; (mark modified read-only locked
                        ;;  ,@(if (modulep! +icons)
                        ;;        `(;; Here you may adjust by replacing :right with :center
                        ;;          ;; or :left According to taste, if you want the icon
                        ;;          ;; further from the name
                        ;;          " " (icon 2 2 :left :elide)
                        ;;          ,(propertize " " 'display `(space :align-to 8)))
                        ;;      '(" "))
                        ;;  (name 18 18 :left :elide)
                        ;;  " " (size 9 -1 :right)
                        ;;  " " (mode 16 16 :left :elide)
                        ;;  ,@(when (require 'ibuffer-vc nil t)
                        ;;      '(" " (vc-status 12 :left)))
                        ;;  " " filename-and-process)
                        (mark " " (name 16 -1) " " filename)
                        )

      )

;;-- popup
(setq jg-ibuffer-popup-rules
      '(
        ("^\*Ibuffer\*$"         :side right  :ttl nil :width  0.5 :quit nil :select t :priority 50)
        ))
(after! jg-popup-init
  (+jg-ui-popup-add-rules 'ibuffer jg-ibuffer-popup-rules)
  )
;;-- end popup

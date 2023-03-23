;;; +vars.el -*- lexical-binding: t; -*-


;;-- popup
(after! jg-ui-reapply-hook-ready
  (+jg-popup-add-spec 'search
                          '(
                            ("^\*Fd\*" :side bottom :ttl 5 :quit t :select t :priority 50)
                            ))
  )
;;-- end popup

;;; +vars.el -*- lexical-binding: t; -*-


;;-- popup
(spec-handling-add! popup nil
                    ('search
                     ("^\*Fd\*" :side bottom :ttl 5 :quit t :select t :priority 50)
                     )
                    )
;;-- end popup

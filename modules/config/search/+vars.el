;;; +vars.el -*- lexical-binding: t; -*-


(spec-handling-add! popup nil
                    '(search
                     ("^\*Fd\*" :side bottom :ttl 5 :quit t :select t :priority 50)
                     )
                    )

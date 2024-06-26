;;; +vars.el -*- lexical-binding: t; -*-

(setq counsel-describe-function-function #'helpful-callable
      counsel-describe-variable-function #'helpful-variable
      counsel-descbinds-function         #'helpful-callable

      )

(setq helpful-max-buffers 5

      )

(spec-handling-add! fold
                    `(helpful
                      :modes (helpful-mode)
                      :triggers (:open-all  ,#'hs-show-all
                                 :close-all ,#'hs-hide-all
                                 :toggle    ,#'hs-toggle-hiding
                                 :open      ,#'hs-show-block
                                 :open-rec  nil
                                 :close     ,#'hs-hide-block
                                 )
                      )
                    )

(spec-handling-add! popup
                    '(helpful
                      ("\*helpful.*?\\*"   :side bottom :ttl nil :height 20 :quit t :select t :priority 150)
                      )
                    )

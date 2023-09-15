;;; +vars.el -*- lexical-binding: t; -*-

(setq counsel-describe-function-function #'helpful-callable
      counsel-describe-variable-function #'helpful-variable
      counsel-descbinds-function         #'helpful-callable)

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

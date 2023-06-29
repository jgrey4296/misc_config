;;; +vars.el -*- lexical-binding: t; -*-

(defvar jg-help-map (make-keymap))
(define-prefix-command 'jg-help-map nil "jgb-help")

(setq Man-switches (string-join (list "-C"
                                      (expand-file-name "terminal/tool_configs/man.conf" doom-user-dir))
                                " ")
      manual-program (executable-find "man")
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

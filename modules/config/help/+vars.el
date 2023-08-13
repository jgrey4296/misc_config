;;; +vars.el -*- lexical-binding: t; -*-

(defvar jg-help-map (make-keymap))
(define-prefix-command 'jg-help-map nil "jgb-help")

(setq Man-switches (string-join (list "-C"
                                      (expand-file-name "terminal/tool_configs/man.conf" doom-user-dir))
                                " ")
      manual-program (executable-find "man")
      )

;;-- woman
(after! woman
  ;; The woman-manpath default value does not necessarily match man. If we have
  ;; man available but aren't using it for performance reasons, we can extract
  ;; it's manpath.
  (when (executable-find "man")
    (setq woman-manpath
          (split-string (cdr (doom-call-process "man" "--path"))
                        path-separator t))))
;;-- end woman

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

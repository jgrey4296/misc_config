;;; +vars.el -*- lexical-binding: t; -*-

(setq counsel-describe-function-function #'helpful-callable
      counsel-describe-variable-function #'helpful-variable
      counsel-descbinds-function         #'helpful-callable)

;;-- wo/man
(setq Man-switches (string-join (list "-C"
                                      (expand-file-name "templates/tools/man.conf" doom-user-dir))
                                " ")
      manual-program (executable-find "man")
      )
(after! woman
  ;; The woman-manpath default value does not necessarily match man. If we have
  ;; man available but aren't using it for performance reasons, we can extract
  ;; it's manpath.
  (when (executable-find "man")
    (setq woman-manpath
          (split-string (cdr (doom-call-process "man" "--path"))
                        path-separator t))))
;;-- end wo/man

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

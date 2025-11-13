;;; +mamba.el -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package! micromamba
  ;; dependencies: pythonic
  :commands (micromamba-activate micromamba-deactivate)
  :init

  (speckler-add! lib-env ()
    :override t
    `(mamba
      :lang 'python
      :start #'jg-py-mamba-start-env
      :stop  #'jg-py-mamba-stop-env
      :modeline #'(lambda (state &rest args) (format "M:%s" (car-safe args)))
      )
    )
  )


;;; +mamba.el ends here

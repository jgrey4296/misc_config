;;; lang/java/+eclim.el -*- lexical-binding: t; -*-

(use-package! eclim
  :hook (java-mode . eclim-mode)
  :config
  (spec-handling-add! lookup-handler nil
                      (java-mode
                       :definition #'eclim-java-find-declaration
                       :references #'eclim-java-find-references
                       :documentation #'eclim-java-show-documentation-for-current-element
                       )
                      )

  (require 'eclimd)
  (setq help-at-pt-display-when-idle t
        help-at-pt-timer-delay 0.1)
  (help-at-pt-set-timer)

  )


(use-package! company-emacs-eclim
  :when (modulep! :completion company)
  :after java-mode
  :config
  (spec-handling-add! company nil (java-mode company-emacs-eclim))
  )

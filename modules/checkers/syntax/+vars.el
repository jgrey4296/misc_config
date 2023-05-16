;;; +vars.el -*- lexical-binding: t; -*-


(setq-default flycheck-display-errors-delay 1
              flycheck-display-errors-function nil
              flycheck-help-echo-function nil
              flycheck-process-error-functions nil

              )

(setq flycheck-emacs-lisp-load-path 'inherit
      flycheck-check-syntax-automatically '(save idle-change mode-enabled)
      flycheck-idle-change-delay 1.0
      flycheck-buffer-switch-check-intermediate-buffers t
      flycheck-display-errors-delay 0.25
      flycheck-popup-tip-error-prefix "X "
      flycheck-posframe-warning-prefix "! "
      flycheck-posframe-info-prefix "··· "
      flycheck-posframe-error-prefix "X "
      )

(spec-handling-add! popup nil
                    '(flycheck
                      ("^\\*Flycheck error messages\\*" :select nil)
                      ("^\\*Flycheck errors\\*" :size 0.25)
                      )
                    )

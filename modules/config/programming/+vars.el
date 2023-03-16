;;; +vars.el -*- lexical-binding: t; -*-

;;-- flycheck
(after! flycheck
  (setq-default flycheck-display-errors-delay 1
                flycheck-display-errors-function nil
                flycheck-help-echo-function nil
                flycheck-process-error-functions nil )
)

;;-- end flycheck

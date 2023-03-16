;;; +vars.el -*- lexical-binding: t; -*-

(setq jg-help-map (make-keymap))
(define-prefix-command 'jg-help-map nil "jgb-help")

(setq Man-switches (string-join (list "-C"
                                      (expand-file-name "terminal/tool_configs/man.conf" doom-user-dir))
                                " ")
      manual-program (executable-find "man")
      )

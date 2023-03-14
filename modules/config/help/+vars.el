;;; +vars.el -*- lexical-binding: t; -*-

(setq Man-switches (string-join (list "-C"
                                      (expand-file-name "terminal/tool_configs/man.conf" doom-user-dir))
                                " ")
      manual-program (executable-find "man")
      )

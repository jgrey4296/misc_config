;;; +vars.el -*- lexical-binding: t; -*-


(defvar +spell-excluded-faces-alist '())

(setq ispell-program-name (or (executable-find "aspell")
                              (executable-find "hunspell")
                              (executable-find "enchant-2")
                              (executable-find "ispell")
                              )

      ispell-extra-args (pcase (f-filename ispell-program-name)
                          ("aspell"   '("--sug-mode=ultra" "--run-together"))
                          ("hunspell" '())
                          ("enchant-2"  '())
                          ("ispell"   '())
                          )

      ispell-personal-dictionary (expand-file-name "terminal/tool_configs/ispell_english" doom-user-dir)
      spell-fu-directory (concat doom-data-dir "spell-fu")
      flyspell-popup-correct-delay 0.8

      flyspell-lazy-idle-seconds 1
      flyspell-lazy-window-idle-seconds 3
      )

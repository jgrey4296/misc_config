;;; +vars.el -*- lexical-binding: t; -*-

(setq-default shell-default-shell 'shell
              shell-protect-eshell-prompt 0
              shell-enable-smart-eshell t
              )

(setq shell-dynamic-complete-functions '(comint-c-a-p-replace-by-expanded-history
                                         shell-environment-variable-completion
                                         shell-command-completion
                                         shell-c-a-p-replace-by-expanded-directory
                                         pcomplete-completions-at-point
                                         shell-filename-completion
                                         comint-filename-completion
                                         )
      )

(setq vterm-kill-buffer-on-exit t ;; Once vterm is dead, the vterm buffer is useless. Why keep it around? We can spawn another if want one.
      vterm-max-scrollback 5000
      )

(setq comint-dynamic-complete-functions '(comint-c-a-p-replace-by-expanded-history
                                          comint-filename-completion)
      ansi-color-for-comint-mode t
      comint-prompt-read-only t
      comint-buffer-maximum-size 2048 ; double the default
      )

(spec-handling-add! popup
                    '(vterm
                      ("^\\*vterm" :size 0.25 :vslot -4 :select t :quit nil :ttl 0)
                      )
                    )
(spec-handling-add! tree-sit-lang
                    '(sh-mode         . bash)
                    )

(spec-handling-add! auto-modes
                    '(shell
                      ("\\.bash\\'" . sh-mode)
                      )
                    )

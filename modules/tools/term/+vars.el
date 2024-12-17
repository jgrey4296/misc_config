;;; +vars.el -*- lexical-binding: t; -*-

;;-- keymaps
(defvar jg-term-shell-mode-map (make-sparse-keymap))

(defvar jg-term-comint-mode-map (make-sparse-keymap))

;;-- end keymaps

;;-- vterm
(setq vterm-kill-buffer-on-exit t
      vterm-max-scrollback 5000
      )

;;-- end vterm

(speckler-add! popup
                    '(shell
                      ("^\\*shell" :side bottom :ttl nil :height 0.3 :quit t :select t :priority 100)
                      )
                    '(vterm
                      ("^\\*vterm" :size 0.25 :vslot -4 :select t :quit nil :ttl 0)

                      )
                    )

(speckler-add! tree-sit-lang
                    '(sh-mode         . bash)
                    )

(speckler-add! auto-modes
                    '(shell
                      ("\\.bash\\'" . sh-mode)
                      )
                    )

(speckler-add! babel
                    '(term
                     (:name bash       :lib ob-shell)
                     (:name eshell     :lib ob-eshell)
                     (:name sh         :lib ob-shell)
                     (:name shell      :lib ob-shell)
                      )
                    )

(speckler-add! org-src
                    '(term
                      ("bash" . sh)
                      ("shell" . sh)
                      ("sh" . sh)
                      ("bash2" . sh)
                      )
                    )

(speckler-setq! shell 50
                     shell-dynamic-complete-functions '(comint-c-a-p-replace-by-expanded-history
                                                        shell-environment-variable-completion
                                                        shell-command-completion
                                                        shell-c-a-p-replace-by-expanded-directory
                                                        pcomplete-completions-at-point
                                                        shell-filename-completion
                                                        comint-filename-completion
                                                        )

                     comint-dynamic-complete-functions '(comint-c-a-p-replace-by-expanded-history
                                                         comint-filename-completion)
                     ansi-color-for-comint-mode t
                     comint-prompt-read-only t
                     comint-buffer-maximum-size 2048 ; double the default
                     )

(speckler-add! company
                    '(shell-mode (:mode company-shell company-shell-env company-files))
                    )

;; transients.el -*- lexical-binding: t; -*-
(require 'transient)
;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/

(progn
  (transient-make-call! lsp-doc-childframe
                        (format "%-2s : Doc Childframe" (fmt-as-bool! lsp-ui-doc-use-childframe))
                        (setq lsp-ui-doc-use-childframe (not lsp-ui-doc-use-childframe))
                        )
  (transient-make-toggle! lsp-modeline-diagnostics-mode      "m" "Modeline Diagnostics")
  (transient-make-call!   lsp-keep-alive
                          (format "%-2s : Keep Alive" (fmt-as-bool! lsp-keep-workspace-alive))
                          (setq lsp-keep-workspace-alive (not lsp-keep-workspace-alive)))
  (transient-make-call!   lsp-trace-io
                          (format "%-2s : Log IO" (fmt-as-bool! lsp-log-io))
                          (with-current-buffer transient--original-buffer
                            (call-interactively #'lsp-toggle-trace-io)
                            ))
  (transient-make-toggle! lsp-ui-sideline-mode               "s" "sideline")
  (transient-make-toggle! lsp-modeline-code-actions-mode     "a" "Modeline code actions")
  (transient-make-toggle! lsp-headerline-breadcrumb-mode     "b" "Breadcrumb")
  (transient-make-toggle! lsp-ui-doc-mode                    "d" "Documentation Popup")
  (transient-make-toggle! lsp-lens-mode                      "l" "Lenses")

  (transient-make-call!   lsp-on-type-formatting
                          (format "%-2s : On Type Formatting" (fmt-as-bool! lsp-enable-on-type-formatting))
                          (with-current-buffer transient--original-buffer
                            (call-interactively #'lsp-toggle-on-type-formatting)))
  (transient-make-call!   lsp-highlighting
                          (format "%-2s : Highlighting" (fmt-as-bool! lsp-enable-symbol-highlighting))
                          (with-current-buffer transient--original-buffer
                            (call-interactively #'lsp-toggle-symbol-highlight)))
  (transient-make-call!   lsp-auto-signature
                          (format "%-2s : Auto-Signature" (fmt-as-bool! lsp-signature-auto-activate))
                          (with-current-buffer transient--original-buffer
                          (call-interactively #'lsp-toggle-signature-auto-activate)))
  (transient-make-toggle! lsp-treemacs-sync-mode             "T" "treemacs integration")

  (transient-make-call! lsp-debug
                        "Debug Handlers"
                        (+jg-ide-debug-lsp))
  (transient-make-call! lsp-select
                        "Select Server"
                        (with-current-buffer transient--original-buffer
                          (call-interactively #'+lsp/switch-client)))
  (transient-make-call! lsp-disconnect
                        "Disconnect Server"
                        (with-current-buffer transient--original-buffer
                          (call-interactively #'lsp-disconnect)))
  (transient-make-call! lsp-shutdown
                        "Shutdown Servers"
                        (dolist (workspace (lsp--session-workspaces lsp--session))
                          (lsp-workspace-shutdown workspace)))

  (transient-make-call! lsp-restart
                        "Restart Server"
                        (with-current-buffer transient--original-buffer
                          (call-interactively #'lsp-workspace-restart)))
  (transient-make-call! lsp-start
                        "Start Server"
                        (with-current-buffer transient--original-buffer
                          (call-interactively #'lsp)))

  )

(transient-make-subgroup! jg-lsp-session-control "1"
                          "LSP Session Control Options"
                          :desc "||  Session Control ||"
                          [ [
                           ("\\" jg-transient-call-lsp-select)
                           ("/" "Describe Session" lsp-describe-session)
                           ("?" jg-transient-call-lsp-debug)
                           ]
                          [
                           ("s" jg-transient-call-lsp-start)
                           ("r" jg-transient-call-lsp-restart)
                           ]
                          [
                           ("d" jg-transient-call-lsp-disconnect)
                           ("D" jg-transient-call-lsp-shutdown)
                           ] ]
                          )

;;;###autoload (autoload #'jg-lsp-toggle "tools/ide-support/autoload/transients" nil t)
(transient-make-subgroup! jg-lsp-toggle "l"
                          "Main controller for ui settings"
                          :desc "|| LSP        ||"
                          [
                           [ "View Control"
                            (jg-transient-toggle-lsp-headerline-breadcrumb-mode)
                            (jg-transient-toggle-lsp-lens-mode)
                            ("S" jg-transient-call-lsp-auto-signature)
                            (jg-transient-toggle-lsp-ui-sideline-mode)
                            (jg-transient-toggle-lsp-ui-doc-mode)
                            ("f" jg-transient-call-lsp-doc-childframe)
                            ]
                           [ "Diagnostics"
                             (jg-transient-toggle-lsp-modeline-diagnostics-mode)
                             (jg-transient-toggle-lsp-modeline-code-actions-mode)
                             ]
                           [ "Settings"
                             jg-lsp-session-control
                             ("F" jg-transient-call-lsp-on-type-formatting)
                             (jg-transient-toggle-lsp-treemacs-sync-mode)
                             ("H" jg-transient-call-lsp-highlighting)

                             ("k" jg-transient-call-lsp-keep-alive)
                             ("i" jg-transient-call-lsp-trace-io)
                             ]
                           ]
                          )

;;;###autoload
(defun +jg-ide-extend-toggles ()
  (transient-append-suffix 'jg-toggle-main "w" jg-lsp-toggle)
  )

;; transients.el -*- lexical-binding: t; -*-
(require 'transient)


(progn
  (transient-make-toggle! lsp-modeline-diagnostics-mode      "m" "Modeline Diagnostics")
  (transient-make-toggle! lsp-toggle-trace-io                "i" "Log Io")
  (transient-make-toggle! lsp-ui-sideline-mode               "s" "sideline")
  (transient-make-toggle! lsp-modeline-code-actions-mode     "a" "Modeline code actions")
  (transient-make-toggle! lsp-headerline-breadcrumb-mode     "b" "Breadcrumb")
  (transient-make-toggle! lsp-ui-doc-mode                    "d" "Documentation Popup")
  (transient-make-toggle! lsp-lens-mode                      "l" "Lenses")

  (transient-make-toggle! lsp-toggle-on-type-formatting      "F" "On Type Formatting")
  (transient-make-toggle! lsp-toggle-symbol-highlight        "H" "Highlighting")
  (transient-make-toggle! lsp-toggle-signature-auto-activate "S" "Auto Signature")
  (transient-make-toggle! lsp-treemacs-sync-mode             "T" "treemacs integration")

  (transient-make-call! lsp-debug
                        "Debug Handlers"
                        (+jg-ide-debug-lsp))
  (transient-make-call! lsp-select
                        "Select Server"
                        (with-current-buffer transient--original-buffer
                          (call-interactively #'+lsp/switch-client)))
  (transient-make-call! lsp-disconnect
                        "Disconnect Sever"
                        (with-current-buffer transient--original-buffer
                          (call-interactively #'lsp-disconnect)))
  (transient-make-call! lsp-shutdown
                        "Shutdown Server"
                        (with-current-buffer transient--original-buffer
                        (call-interactively #'lsp-workspace-shutdown)))
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
(transient-make-subgroup! jg-toggle-lsp "l"
                          "Main controller for ui settings"
                          :desc "|| LSP        ||"
                          [
                            [ jg-lsp-session-control
                             (jg-transient-toggle-lsp-headerline-breadcrumb-mode)
                             (jg-transient-toggle-lsp-modeline-diagnostics-mode)
                             (jg-transient-toggle-lsp-ui-doc-mode)
                             (jg-transient-toggle-lsp-ui-sideline-mode)
                             ]
                            [ " "
                             (jg-transient-toggle-lsp-lens-mode)
                             (jg-transient-toggle-lsp-modeline-code-actions-mode)
                             (jg-transient-toggle-lsp-toggle-trace-io)
                             ]
                            [ " "
                             (jg-transient-toggle-lsp-toggle-on-type-formatting)
                             (jg-transient-toggle-lsp-toggle-symbol-highlight)
                             (jg-transient-toggle-lsp-toggle-signature-auto-activate)
                             (jg-transient-toggle-lsp-treemacs-sync-mode)
                             ]
                            ]
                          )

;; transients.el -*- lexical-binding: t; -*-
(require 'transient)
;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/

(progn
  (transient-make-var-toggle! lsp-doc-hover lsp-eldoc-enable-hover         "Docs on Hover" "h")
  (transient-make-var-toggle! lsp-doc-childframe lsp-ui-doc-use-childframe "Doc Childframe" "f")
  (transient-make-var-toggle! lsp-keep-alive lsp-keep-workspace-alive      "Keep Alive" "k")

  (transient-make-mode-toggle! lsp-ui-sideline-mode                        "Sideline" "s")
  (transient-make-mode-toggle! lsp-modeline-code-actions-mode              "Modeline code actions" "a")
  (transient-make-mode-toggle! lsp-headerline-breadcrumb-mode              "Breadcrumb" "b")
  (transient-make-mode-toggle! lsp-ui-doc-mode                             "Documentation Popup" "p")
  (transient-make-mode-toggle! lsp-lens-mode                               "Lenses" "l")
  (transient-make-mode-toggle! lsp-treemacs-sync-mode                      "Treemacs integration" "T")

  (transient-make-mode-toggle! tree-sitter-hl-mode                         "TreeSitter HL" "e")
  (transient-make-mode-toggle! lsp-modeline-diagnostics-mode               "Modeline Diagnostics" "m")
  (transient-make-mode-toggle! lsp-diagnostics-mode                        "Diagnostics" "d")
  (transient-make-mode-toggle! lsp-completion-mode                         "Completion" "c")

  ;;
  (transient-make-call! lsp-trace-io "i"
                        (format "%-2s : Log IO" (fmt-as-bool! lsp-log-io))
                        (call-interactively #'lsp-toggle-trace-io))
  (transient-make-call! lsp-on-type-formatting "F"
                        (format "%-2s : On Type Formatting" (fmt-as-bool! lsp-enable-on-type-formatting))
                        (call-interactively #'lsp-toggle-on-type-formatting))
  (transient-make-call! lsp-highlighting "H"
                        (format "%-2s : Highlighting" (fmt-as-bool! lsp-enable-symbol-highlighting))
                        (call-interactively #'lsp-toggle-symbol-highlight))
  (transient-make-call! lsp-auto-signature "S"
                        (format "%-2s : Auto-Signature" (fmt-as-bool! lsp-signature-auto-activate))
                        (call-interactively #'lsp-toggle-signature-auto-activate))
  (transient-make-call! lsp-debug "?"
                        "Debug Handlers"
                        (+jg-ide-debug-lsp))
  (transient-make-call! lsp-select "\\"
                        "Select Server"
                        (call-interactively #'+lsp/switch-client))
  (transient-make-call! lsp-disconnect "d"
                        "Disconnect Server"
                        (call-interactively #'lsp-disconnect))
  (transient-make-call! lsp-shutdown "D"
                        "Shutdown Servers"
                        (dolist (workspace (lsp--session-workspaces lsp--session))
                          (lsp-workspace-shutdown workspace)))
  (transient-make-call! lsp-restart "r"
                        "Restart Server"
                        (call-interactively #'lsp-workspace-restart))
  (transient-make-call! lsp-start "s"
                        "Start Server"
                        (call-interactively #'lsp))

  (transient-make-call! lsp-remove "-"
                        "Remove LSP Workspace"
                        (call-interactively #'lsp-workspace-folders-remove))

  (transient-make-call! lsp-add "="
                        "Add Folder"
                        (call-interactively #'lsp-workspace-folders-add))

  (transient-make-call! lsp-list-blacklist "b"
                        "Blacklisted"
                        (let ((blacklist (lsp-session-folders-blacklist (lsp-session))))
                          (with-temp-buffer-window "*LSP: Blacklisted*" #'popup-window nil
                            (dolist (folder blacklist)
                              (princ (format "-- %s\n" folder))))))
  (transient-make-call! lsp-unblacklist "B"
                        "Unblacklist folder"
                        (call-interactively #'lsp-workspace-blacklist-remove))

  (transient-make-call! lsp-clients "p"
                        "Registered Clients"
                        (call-interactively #'+jg-ide-registered-lsp-clients))
  (transient-make-call! lsp-disable-client "["
                        "Disable client"
                        (call-interactively #'+jg-ide-disable-lsp-client))
  (transient-make-call! lsp-enable-client "]"
                        "Enable Client"
                        (call-interactively #'+jg-ide-enable-lsp-client))

  )

(transient-make-subgroup! jg-lsp-session-control "1"
                          "LSP Session Control Options"
                          :desc "||  Session Control ||"
                          [
                           [
                            ("0" "Describe Session" lsp-describe-session)
                            (transient-macro-call-lsp-clients)
                            (transient-macro-call-lsp-list-blacklist)
                            ]
                           [(transient-macro-call-lsp-remove)
                            (transient-macro-call-lsp-disable-client)
                            (transient-macro-call-lsp-unblacklist)
                            ]
                           [(transient-macro-call-lsp-add)
                            (transient-macro-call-lsp-enable-client)]
                           ]
                          [ [
                           (transient-macro-call-lsp-select)
                           (transient-macro-call-lsp-debug)
                           ]
                          [
                           (transient-macro-call-lsp-start)
                           (transient-macro-call-lsp-restart)
                           ]
                          [
                           (transient-macro-call-lsp-disconnect)
                           (transient-macro-call-lsp-shutdown)
                           ] ]
                          )

;;;###autoload (autoload #'jg-lsp-toggle "ide/support/autoload/transients" nil t)
(transient-make-subgroup! jg-lsp-toggle "l"
                          "Main controller for ui settings"
                          :desc "|| LSP        ||"
                          [jg-lsp-session-control]
                          [[ "View Control"
                            (transient-macro-toggle-lsp-headerline-breadcrumb-mode)
                            (transient-macro-toggle-lsp-lens-mode)
                            (transient-macro-call-lsp-auto-signature)
                            (transient-macro-toggle-lsp-ui-sideline-mode)
                            (transient-macro-toggle-lsp-ui-doc-mode)
                            (transient-macro-toggle-lsp-doc-childframe)
                            ]
                           [ "Diagnostics"
                             (transient-macro-toggle-lsp-modeline-diagnostics-mode)
                             (transient-macro-toggle-lsp-modeline-code-actions-mode)
                           " "
                           "Toggles"
                            (transient-macro-toggle-lsp-doc-hover)
                            (transient-macro-toggle-lsp-completion-mode)
                            (transient-macro-toggle-lsp-diagnostics-mode)
                            (transient-macro-toggle-tree-sitter-hl-mode)
                            ]
                           [ "Settings"
                             (transient-macro-call-lsp-on-type-formatting)
                             (transient-macro-toggle-lsp-treemacs-sync-mode)
                             (transient-macro-call-lsp-highlighting)

                             (transient-macro-toggle-lsp-keep-alive)
                             (transient-macro-call-lsp-trace-io)
                             ]
                           ]
                          )

;;;###autoload
(defun +jg-ide-extend-toggles ()
  (transient-append-suffix 'jg-toggle-main "w" jg-lsp-toggle)
  )

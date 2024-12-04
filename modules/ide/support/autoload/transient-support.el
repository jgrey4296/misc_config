;; transients.el -*- lexical-binding: t; -*-
(require 'transient)
;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/

(progn
  (transient-make-var-toggle! lsp-doc-hover lsp-eldoc-enable-hover         "Docs on Hover"         "h")
  (transient-make-var-toggle! lsp-doc-childframe lsp-ui-doc-use-childframe "Doc Childframe"        "f")
  (transient-make-var-toggle! lsp-keep-alive lsp-keep-workspace-alive      "Keep Alive"            "k")

  (transient-make-mode-toggle! lsp-ui-sideline-mode                        "Sideline"              "s")
  (transient-make-mode-toggle! lsp-modeline-code-actions-mode              "Modeline code actions" "a")
  (transient-make-mode-toggle! lsp-headerline-breadcrumb-mode              "Breadcrumb"            "b")
  (transient-make-mode-toggle! lsp-ui-doc-mode                             "Documentation Popup"   "p")
  (transient-make-mode-toggle! lsp-lens-mode                               "Lenses"                "l")
  (transient-make-mode-toggle! lsp-treemacs-sync-mode                      "Treemacs integration"  "T")

  (transient-make-mode-toggle! tree-sitter-hl-mode                         "TreeSitter HL"         "e")
  (transient-make-mode-toggle! lsp-modeline-diagnostics-mode               "Modeline Diagnostics"  "m")
  (transient-make-mode-toggle! lsp-diagnostics-mode                        "Diagnostics"           "d")
  (transient-make-mode-toggle! lsp-completion-mode                         "Completion"            "c")

  (transient-make-mode-toggle! tree-sitter-hl-mode                         "Tree Sitter HL"        "t")

  )

(progn
  ;;
  (transient-make-int-call! lsp-trace-io "i"
                            (transient-title-var-formatter "Log IO" lsp-log-io "i")
                            #'lsp-toggle-trace-io)
  (transient-make-int-call! lsp-on-type-formatting "F"
                            (transient-title-var-formatter "On Type Formatting" lsp-enable-on-type-formatting  "F")
                            #'lsp-toggle-on-type-formatting)
  (transient-make-int-call! lsp-highlighting "H"
                            (transient-title-var-formatter "Highlighting" lsp-enable-symbol-highlighting "H")
                            #'lsp-toggle-symbol-highlight)
  (transient-make-int-call! lsp-auto-signature "S"
                            (transient-title-var-formatter "Auto-Signature"  lsp-signature-auto-activate "S")
                            #'lsp-toggle-signature-auto-activate)
  (transient-make-call! lsp-debug "?"
                        "Debug Handlers"
                        (+jg-ide-debug-lsp))
  (transient-make-int-call! lsp-select "\\"
                            "Select Server"
                            #'+lsp/switch-client)
  (transient-make-int-call! lsp-disconnect "d"
                            "Disconnect Server"
                            #'lsp-disconnect)
  (transient-make-call! lsp-shutdown "D"
                        "Shutdown Servers"
                        (dolist (workspace (lsp--session-workspaces lsp--session))
                          (lsp-workspace-shutdown workspace)))
  (transient-make-int-call! lsp-restart "r"
                            "Restart Server"
                            #'lsp-workspace-restart)
  (transient-make-int-call! lsp-start "s"
                            "Start Server"
                            #'lsp)

  (transient-make-int-call! lsp-remove "-"
                            "Remove LSP Workspace"
                            #'lsp-workspace-folders-remove)

  (transient-make-int-call! lsp-add "="
                            "Add Folder"
                            #'lsp-workspace-folders-add)

  (transient-make-call! lsp-list-blacklist "b"
                        "Blocklisted"
                        (let ((blocklist (lsp-session-folders-blocklist (lsp-session))))
                          (with-temp-buffer-window "*LSP: Blocklisted*" #'popup-window nil
                            (dolist (folder blocklist)
                              (princ (format "-- %s\n" folder))))))
  (transient-make-int-call! lsp-unblocklist "B"
                            "Unblocklist folder"
                            #'lsp-workspace-blocklist-remove)

  (transient-make-int-call! lsp-clients "p"
                            "Registered Clients"
                            #'+jg-ide-registered-lsp-clients)
  (transient-make-int-call! lsp-disable-client "["
                            "Disable client"
                            #'+jg-ide-disable-lsp-client)
  (transient-make-int-call! lsp-enable-client "]"
                            "Enable Client"
                            #'+jg-ide-enable-lsp-client)
  )

;;;###autoload
(defun +jg-ide-build-lsp-transient ()
  (transient-make-subgroup! jg-lsp-session-control "1"
                            "LSP Session Control Options"
                            :desc "|| Session Control ||"
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

  (transient-make-subgroup! jg-lsp-toggle "l"
                            "Main controller for ui settings"
                            :desc "|| LSP        ||"
                            [[ jg-lsp-session-control ]]
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

  (pcase (transient-get-suffix 'jg-toggle-main '(1 -1))
    ((and `[1 transient-columns nil ,x]
          (guard (< (length x) 4)))
     (transient-append-suffix 'jg-toggle-main
       '(1 -1 -1) jg-lsp-toggle))
    (t (transient-append-suffix 'jg-toggle-main
         '(1 -1) [ jg-lsp-toggle ]))
    )

  (transient-append-suffix (caddr jg-toggle-visuals-transient)
    "H" '("t" transient-macro-toggle-tree-sitter-hl-mode))
  )

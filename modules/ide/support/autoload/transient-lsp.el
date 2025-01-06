;; transient-lsp.el -*- lexical-binding: t; -*-
(require 'macro-tools--transient)

(transient-toggle-var! lsp-doc-hover ()
  ""
  :var lsp-eldoc-enable-hover
  :desc "ElDoc on Hover"
  :key "h"
  )
(transient-toggle-var! lsp-doc-childframe ()
  ""
  :var lsp-ui-doc-use-childframe
  :desc "Doc Childframe"
  :key "f"
  )
(transient-toggle-var! lsp-keep-alive ()
  ""
  :var lsp-keep-workspace-alive
  :desc "Keep Alive"
  :key "k"
  )

(transient-toggle-hook! lsp-ui-sideline ()
  ""
  :hook 'lsp-mode
  :fn   #'lsp-ui-sideline-mode
  :desc "Sideline"
  :key "s"
  )
(transient-toggle-hook! lsp-code-actions ()
  ""
  :hook 'lsp-mode
  :fn   #'lsp-modeline-code-actions-mode
  :desc "Modeline code actions"
  :key "a"
  )
(transient-toggle-hook! lsp-breadcrumb ()
  ""
  :hook 'lsp-mode
  :fn #'lsp-headerline-breadcrumb-mode
  :desc "Breadcrumb"
  :key "b"
  )
(transient-toggle-hook! lsp-doc-popup ()
  ""
  :hook 'lsp-mode
  :fn #'lsp-ui-doc-mode
  :desc "Documentation Popup"
  :key "p"
  )
(transient-toggle-hook! lsp-lenses ()
  ""
  :hook 'lsp-mode
  :fn #'lsp-lens-mode
  :desc "Lenses"
  :key "l"
  )
(transient-toggle-hook! lsp-treemacs ()
  ""
  :hook 'lsp-mode
  :fn #'lsp-treemacs-sync-mode
  :desc "Treemacs integration"
  :key "T"
  )
(transient-toggle-hook! lsp-imenu ()
  "Use lsp's imenu instead "
  :hook 'lsp-mode
  :fn #'lsp-imenu-override
  :desc "Pretty LSP Imenu"
  :key "M"
  )

(transient-toggle-hook! lsp-modeline-diagnostics ()
  ""
  :hook 'lsp-mode
  :fn #'lsp-modeline-diagnostics-mode
  :desc "Modeline Diagnostics"
  :key "m"
  )
(transient-toggle-hook! lsp-diagnostics ()
  ""
  :hook 'lsp-mode
  :fn #'lsp-diagnostics-mode
  :desc "Diagnostics"
  :key "d"
  )
(transient-toggle-hook! lsp-completion ()
  ""
  :hook 'lsp-mode
  :fn #'lsp-completion-mode
  :desc "Completion"
  :key "c"
  )

(transient-toggle-hook! tree-sitter-hl ()
  ""
  :hook 'lsp-mode
  :fn #'tree-sitter-hl-mode
  :desc "Tree Sitter HL"
  :key "t"
  )


(transient-call! lsp-trace-io ()
  "Activate LSP Logging"
  :key "i"
  :desc (transient-var-fmt "Log IO" lsp-log-io "i")
  :interactive t
  #'lsp-toggle-trace-io
  )
(transient-call! lsp-on-type-formatting ()
  "Activate on type formatting"
  :key "F"
  :desc (transient-var-fmt "On Type Formatting" lsp-enable-on-type-formatting  "F")
  :interactive t
  #'lsp-toggle-on-type-formatting
  )
(transient-call! lsp-highlighting ()
  "Activate LSP Highlighting"
  :key "H"
  :desc (transient-var-fmt "Highlighting" lsp-enable-symbol-highlighting "H")
  :interactive t
  #'lsp-toggle-symbol-highlight
  )
(transient-call! lsp-auto-signature ()
  ""
  :key "S"
  :desc (transient-var-fmt "Auto-Signature"  lsp-signature-auto-activate "S")
  :interactive t
  #'lsp-toggle-signature-auto-activate
  )
(transient-call! lsp-debug ()
  "Debug :LSP doc handlers"
  :key "?"
  :desc "Debug Handlers"
  (+jg-ide-debug-lsp)
  )
(transient-call! lsp-select ()
  ""
  :key "\\"
  :desc "Select Server"
  :interactive t
  #'+lsp/switch-client
  )
(transient-call! lsp-disconnect ()
  ""
  :key "d"
  :desc "Disconnect Server"
  :interactive t
  )
(transient-call! lsp-shutdown ()
  ""
  :key "D"
  :desc "Shutdown Servers"
  (dolist (workspace (lsp--session-workspaces lsp--session))
    (lsp-workspace-shutdown workspace))
  )
(transient-call! lsp-restart ()
  ""
  :key "r"
  :desc "Restart Server"
  :interactive t
  #'lsp-workspace-restart
  )
(transient-call! lsp-start ()
  ""
  :key "s"
  :desc "Start Server"
  :interactive t
  #'lsp
  )
(transient-call! lsp-remove ()
  ""
  :key "-"
  :desc "Remove LSP Workspace"
  :interactive t
  #'lsp-workspace-folders-remove
  )
(transient-call! lsp-add ()
  ""
  :key "="
  :desc "Add Folder"
  :interactive t
  #'lsp-workspace-folders-add
  )
(transient-call! lsp-list-blacklist ()
  ""
  :key "b"
  :desc "Blocklisted"
  (let ((blocklist (lsp-session-folders-blocklist (lsp-session))))
    (with-temp-buffer-window "*LSP: Blocklisted*" #'popup-window nil
      (dolist (folder blocklist)
        (princ (format "-- %s\n" folder)))))
  )
(transient-call! lsp-unblacklist ()
  ""
  :key "B"
  :desc "Unblocklist folder"
  :interactive t
  #'lsp-workspace-blocklist-remove
  )
(transient-call! lsp-clients ()
  ""
  :key "p"
  :desc "Registered Clients"
  :interactive t
  #'+jg-ide-registered-lsp-clients
  )
(transient-call! lsp-disable-client ()
  ""
  :key "["
  :desc "Disable client"
  :interactive t
  #'+jg-ide-disable-lsp-client
  )
(transient-call! lsp-enable-client ()
  ""
  :key "]"
  :desc "Enable Client"
  :interactive t
  #'+jg-ide-enable-lsp-client
  )

(transient-subgroup! jg-lsp-session-control ()
  "LSP Session Control Options"
  :key "1"
  :desc "|| Session Control ||"
  :rows t
  ;; Row 1
  ["|| Session Control ||"
   [("0" "Describe Session" lsp-describe-session)
   (transient-macro-call-lsp-clients)
   (transient-macro-call-lsp-list-blacklist)
   ]
  [(transient-macro-call-lsp-remove)
   (transient-macro-call-lsp-disable-client)
   (transient-macro-call-lsp-unblacklist)
   ]
  ]
  ;; Row 2
  ["----"
   [(transient-macro-call-lsp-add)
    (transient-macro-call-lsp-enable-client)
    ]
   [(transient-macro-call-lsp-select)
    (transient-macro-call-lsp-debug)
    ]
   ]
  ;; Row 3
  ["----"
   [
   (transient-macro-call-lsp-start)
   (transient-macro-call-lsp-restart)
   ]
  [
   (transient-macro-call-lsp-disconnect)
   (transient-macro-call-lsp-shutdown)
   ]
  ]
  )

(transient-subgroup! jg-lsp-toggle ()
  "Main controller for ui settings"
  :key "l"
  :desc "|| LSP        ||"
  :rows t
  [:description jg-lsp-toggle-descfn
                " "
                jg-lsp-session-control
                ]
  [[ "View Control"
    (transient-macro-toggle-hook-lsp-breadcrumb)
    (transient-macro-toggle-hook-lsp-lenses)
    (transient-macro-toggle-hook-lsp-ui-sideline)
    (transient-macro-toggle-hook-lsp-doc-popup)
    (transient-macro-toggle-lsp-doc-childframe)
    (transient-macro-call-lsp-auto-signature)
    ]
   [
    "Toggles"
    (transient-macro-toggle-hook-lsp-treemacs)
    (transient-macro-toggle-hook-lsp-completion)
    (transient-macro-toggle-hook-tree-sitter-hl)
    (transient-macro-toggle-hook-lsp-imenu)
    (transient-macro-toggle-lsp-doc-hover)
    ]
  [ "Settings"
    (transient-macro-call-lsp-on-type-formatting)
    (transient-macro-call-lsp-highlighting)
    (transient-macro-toggle-lsp-keep-alive)
    (transient-macro-call-lsp-trace-io)
    " "
    "Diagnostics"
    (transient-macro-toggle-hook-lsp-modeline-diagnostics)
    (transient-macro-toggle-hook-lsp-diagnostics)
    (transient-macro-toggle-hook-lsp-code-actions)
    ]]
  )

;;;###autoload
(defun +jg-ide-build-lsp-transient ()
  (transient-guarded-insert! 'jg-toggle-main jg-lsp-toggle (1 -1))

  (transient-append-suffix (cadr jg-toggle-visuals-transient)
    "H" '("t" transient-macro-toggle-hook-tree-sitter-hl)
    )

  )

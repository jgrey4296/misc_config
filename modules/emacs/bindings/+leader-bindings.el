;; Leader no prefix
;;
(map! :leader
      :desc "Goto-line"             "SPC" #'evil-avy-goto-line
      :desc "Ibuffer"               "DEL" #'ibuffer

      :desc "Find file"             "."    #'find-file
      :desc "Switch buffer"         ","    #'switch-to-buffer

      :desc "Evaluate line/region"  "e"   #'eval-last-sexp
      :desc "Eval expression"       ";"   #'pp-eval-expression
      :desc "M-x"                   ":"   #'execute-extended-command
      :desc "Org Capture"           "X"   #'org-capture
      :desc "Pop Shell"             "'"   #'shell

      ;; C-u is used by evil
      :desc "Universal argument"    "u"   #'universal-argument
      :desc "Open Url"              "U"   #'jg-browse-url

      :desc "help"                  "h"    help-map

      :desc "Split Window"          "/"    #'split-window-right
      ;; TODO pop shell

      (:when (featurep! :ui workspaces)
       :desc "Switch workspace buffer" "," #'persp-switch-to-buffer
       :desc "Switch buffer"           "<" #'switch-to-buffer)

      :desc "Switch to last buffer" "TAB"    #'evil-switch-to-windows-last-buffer
      ;; :desc "Resume last search"    "'"
      ;; (cond ((featurep! :completion ivy)   #'ivy-resume)
      ;;       ((featurep! :completion helm)  #'helm-resume))

      :desc "Jump to bookmark"      "RET"  #'bookmark-jump

      (:when (featurep! :ui popup)
       :desc "Toggle last popup"     "~"    #'+popup/toggle)

      (:prefix ("!" . "checkers"))      ; bound by flycheck

      :desc "Desktop"   "1" #'+jg-bindings-goto-desktop
      :desc "Github"    "2" #'+jg-bindings-goto-github
      :desc "Mega"      "3" #'+jg-bindings-goto-mega
      :desc "Home"      "4" #'+jg-bindings-goto-home
      :desc "Resources" "5" #'+jg-bindings-goto-resources

      :desc "Agenda"    "7" #'+jg-bindings-goto-org-agenda-file
      :desc "Twitter"   "8" #'jg-browse-twitter
      ;; :desc "Mail"      "9" #'mu4e
      :desc "Messages"  "0" #'+jg-bindings-goto-messages
      )
;;; <leader> a -- misc
(map! :leader
     (:prefix ("a" . "JG Misc")
      :desc "Minibuffer history" "m" #'counsel-minibuffer-history
      )
)
;;; <leader> b --- buffer
(map! :leader
      :prefix ("b" . "buffer")
      :desc "Bury buffer"                 "z"   #'bury-buffer

      :desc "Clear Buffer"                "c" #'+jg-bindings-clear-buffer
      :desc "Kill all buffers"            "K" #'doom/kill-all-buffers
      :desc "Kill buffer"                 "d" #'kill-current-buffer
      :desc "Kill other buffers"          "O" #'doom/kill-other-buffers
      :desc "New empty buffer"            "N" #'evil-buffer-new
      :desc "Next buffer"                 "]" #'next-buffer
      :desc "Pop up scratch buffer"       "x" #'doom/open-scratch-buffer
      :desc "Previous buffer"             "[" #'previous-buffer
      :desc "Read-only mode"              "r" #'read-only-mode
      :desc "Revert buffer"               "R" #'revert-buffer
      :desc "Save all buffers"            "S" #'evil-write-all
      :desc "Save buffer"                 "s" #'basic-save-buffer
      :desc "Switch to last buffer"       "l" #'evil-switch-to-windows-last-buffer
      :desc "Pop up scratch buffer"       "x" #'doom/open-scratch-buffer
      :desc "Switch to scratch buffer"    "X" #'doom/switch-to-scratch-buffer
      :desc "Toggle narrowing"            "-" #'jg-toggle-narrow-buffer
      :desc "Clone Indirect"              "i" #'clone-indirect-buffer-other-window

      (:when (featurep! :ui workspaces)
       :desc "Switch workspace buffer" "b" #'persp-switch-to-buffer
       :desc "Switch buffer"           "B" #'switch-to-buffer)
      (:when (not (featurep! :ui workspaces))
       :desc "Switch buffer"           "b" #'switch-to-buffer)
      )
;;; <leader> c --- code
(map! :leader
      :prefix ("c" . "code")
      :desc "Compile"                               "c"   #'compile
      :desc "Evaluate & replace region"             "E"   #'+eval:replace-region
      :desc "Evaluate buffer/region"                "e"   #'+eval/buffer-or-region
      :desc "Find implementations"                  "i"   #'+lookup/implementations
      :desc "Find type definition"                  "t"   #'+lookup/type-definition
      :desc "Format buffer/region"                  "f"   #'+format/region-or-buffer
      :desc "List errors"                           "x"   #'flymake-show-diagnostics-buffer
      :desc "Recompile"                             "C"   #'recompile
      :desc "Send to repl"                          "s"   #'+eval/send-region-to-repl
      (:when (featurep! :checkers syntax)
       :desc "List errors"                         "x"   #'flycheck-list-errors)
      (:when (and (featurep! :tools lsp) (not (featurep! :tools lsp +eglot)))
       :desc "LSP Code actions"                      "a"   #'lsp-execute-code-action
       :desc "LSP Organize imports"                  "o"   #'lsp-organize-imports
       :desc "LSP Rename"                            "r"   #'lsp-rename
       :desc "LSP"                                   "l"   #'+default/lsp-command-map
       (:when (featurep! :completion ivy)
        :desc "Jump to symbol in current workspace" "j"   #'lsp-ivy-workspace-symbol
        :desc "Jump to symbol in any workspace"     "J"   #'lsp-ivy-global-workspace-symbol)
       (:when (featurep! :completion helm)
        :desc "Jump to symbol in current workspace" "j"   #'helm-lsp-workspace-symbol
        :desc "Jump to symbol in any workspace"     "J"   #'helm-lsp-global-workspace-symbol))
      (:when (featurep! :tools lsp +eglot)
       :desc "LSP Execute code action"             "a" #'eglot-code-actions
       :desc "LSP Rename"                          "r" #'eglot-rename
       :desc "LSP Find declaration"                "j" #'eglot-find-declaration)
      :desc "Macro Expand"                         "m" #'pp-macroexpand-last-sexp
)
;;; <leader> f --- file
(map! :leader
      :prefix ("f" . "file")
      (:when (featurep! :tools editorconfig)
       :desc "Open project editorconfig"  "c"   #'editorconfig-find-current-editorconfig)
      :desc "Browse emacs.d"              "E"   #'+default/browse-emacsd
      :desc "Browse private config"       "P"   #'doom/open-private-config
      :desc "Copy this file"              "C"   #'doom/copy-this-file
      :desc "Delete this file"            "D"   #'doom/delete-this-file
      :desc "Find file from here"         "F"   #'+default/find-file-under-here
      :desc "Find file in emacs.d"        "e"   #'+default/find-in-emacsd
      :desc "Find file in private config" "p"   #'doom/open-private-config
      :desc "Find file"                   "f"   #'find-file
      :desc "Locate file"                 "l"   #'locate
      :desc "Open project editorconfig"   "c"   #'editorconfig-find-current-editorconfig
      :desc "Open scratch buffer"         "x"   #'doom/open-scratch-buffer
      :desc "Recent files"                "r"   #'recentf-open-files
      :desc "Recent project files"        "R"   #'projectile-recentf
      :desc "Rename/move file"            "M"   #'doom/move-this-file
      :desc "Save file as..."             "S"   #'write-file
      :desc "Save file"                   "s"   #'save-buffer
      :desc "Sudo find file"              "U"   #'doom/sudo-find-file
      :desc "Switch to scratch buffer"    "X"   #'doom/switch-to-scratch-buffer
      :desc "Yank filename"               "n"   #'+default/yank-buffer-filename
      )
;;; <leader> g --- git
(map! :leader
      :prefix ("g" . "git")
      :desc "Git revert file"             "R"   #'vc-revert
      (:when (featurep! :ui vc-gutter)
       :desc "Git revert hunk"            "r"   #'git-gutter:revert-hunk
       :desc "Git stage hunk"             "s"   #'git-gutter:stage-hunk
       :desc "Git time machine"           "t"   #'git-timemachine-toggle
       :desc "Jump to next hunk"          "n"   #'git-gutter:next-hunk
       :desc "Jump to previous hunk"      "p"   #'git-gutter:previous-hunk)
      (:when (featurep! :tools magit)
       :desc "Magit dispatch"             "/"   #'magit-dispatch
       :desc "Forge dispatch"             "'"   #'forge-dispatch
       :desc "Magit status"               "s"   #'magit-status
       :desc "Magit status here"          "S"   #'magit-status-here
       :desc "Magit blame"                "B"   #'magit-blame-addition
       :desc "Magit fetch"                "F"   #'magit-fetch
       :desc "Magit buffer log"           "L"   #'magit-log
       (:prefix ("f" . "find")
        :desc "Find file"                 "f"   #'magit-find-file
        :desc "Find gitconfig file"       "g"   #'magit-find-git-config-file
        :desc "Find commit"               "c"   #'magit-show-commit
        :desc "Find issue"                "i"   #'forge-visit-issue
        :desc "Find pull request"         "p"   #'forge-visit-pullreq)
       (:prefix ("o" . "open in browser")
        :desc "Browse file or region"     "."   #'browse-at-remote
        :desc "Browse homepage"           "h"   #'+vc/browse-at-remote-homepage
        :desc "Browse remote"             "r"   #'forge-browse-remote
        :desc "Browse commit"             "c"   #'forge-browse-commit
        :desc "Browse an issue"           "i"   #'forge-browse-issue
        :desc "Browse a pull request"     "p"   #'forge-browse-pullreq
        :desc "Browse issues"             "I"   #'forge-browse-issues
        :desc "Browse pull requests"      "P"   #'forge-browse-pullreqs)
       (:prefix ("l" . "list")
        (:when (featurep! :tools gist)
         :desc "List gists"               "g"   #'gist-list)
        :desc "List repositories"         "r"   #'magit-list-repositories
        :desc "List submodules"           "s"   #'magit-list-submodules
        :desc "List issues"               "i"   #'forge-list-issues
        :desc "List pull requests"        "p"   #'forge-list-pullreqs
        :desc "List notifications"        "n"   #'forge-list-notifications)
       (:prefix ("c" . "create")
        :desc "Initialize repo"           "r"   #'magit-init
        :desc "Clone repo"                "R"   #'magit-clone
        :desc "Commit"                    "c"   #'magit-commit-create
        :desc "Fixup"                     "f"   #'magit-commit-fixup
        :desc "Issue"                     "i"   #'forge-create-issue
        :desc "Pull request"              "p"   #'forge-create-pullreq))
      )
;;; <leader> i --- insert
(map! :leader
      :prefix ("i" . "insert")
      :desc "Current file name"             "f"   #'+default/insert-file-path
      :desc "Current file path"             "F"   (cmd!! #'+default/insert-file-path t)
      :desc "Evil ex path"                  "p"   (cmd! (evil-ex "R!echo "))
      :desc "From clipboard"                "y"   #'+default/yank-pop
      :desc "From evil register"            "r"   #'evil-ex-registers
      ;; TODO lorem ipsum
      ;; TODO password-generator
      ;; TODO uuid
      ;; reserve "d" for inserting debug statement by mode
      (:when (featurep! :editor snippets)
       :desc "Snippet"                       "s"   #'yas-insert-snippet)
      :desc "Unicode"                       "u"   #'unicode-chars-list-chars
      )
;;; <leader> j -- Jumping
(map! :leader
      :prefix ("j" . "Jump")
      :desc "Jump to Line"                          "l" #'evil-avy-goto-line
      :desc "Jump to definition"                    "d" #'+lookup/definition
      :desc "Jump to references"                    "D" #'+lookup/references
      :desc "Find implementations"                  "i" #'+lookup/implementations
      :desc "Jump to documentation"                 "k" #'+lookup/documentation
      :desc "Find type definition"                  "t" #'+lookup/type-definition
      :desc "Browse URL"                            "u" #'jg-browse-url
      (:prefix ("b" . "Bookmark")
       :desc "Set bookmark"                "m"           #'bookmark-set
       :desc "Delete bookmark"             "M"           #'bookmark-delete
       :desc "Rename bookmark"             "r"  #'bookmark-rename
       :desc "Save Bookmarks"              "s" #'bookmark-save
       :desc "Load Bookmarks"              "l" #'bookmark-load
       )
      (:when (featurep! :completion ivy)
       :desc "Jump to symbol in current workspace" "j"  #'lsp-ivy-workspace-symbol
       :desc "Jump to symbol in any workspace"     "J"  #'lsp-ivy-global-workspace-symbol)
      (:when (featurep! :completion helm)
       :desc "Jump to symbol in current workspace" "j"  #'helm-lsp-workspace-symbol
       :desc "Jump to symbol in any workspace"     "J"  #'helm-lsp-global-workspace-symbol)
      )
;;; <leader> m -- Local Mode
(map! (:prefix ("m" . "Local Mode")))
;;; <leader> M -- Macros
(map! :leader
      :prefix ("M" . "Macros")
      (:prefix ("c" . "Counter")
       :desc "Increment counter" "a"            #'kmacro-add-counter
       :desc "Insert counter" "c"               #'kmacro-insert-counter
       :desc "Set counter..." "C"               #'kmacro-set-counter
       :desc "Set display format..." "f"        #'kmacro-set-format)
      (:prefix ("e" . "Edit")
       :desc "Assign key binding..." "b"        #'kmacro-bind-to-key
       :desc "Edit last macro" "e"              #'kmacro-edit-macro-repeat
       :desc "Create macro from lossage..." "l" #'kmacro-edit-lossage
       :desc "Name last macro..." "n"           #'kmacro-name-last-macro
       :desc "Write macro to register..." "r"   #'kmacro-to-register
       :desc "Step by step edit..." "s"         #'kmacro-step-edit-macro
       :desc "Start macro/Insert counter" "k"   #'kmacro-start-macro-or-insert-counter
       :desc "Stop or Run" "K"                  #'kmacro-end-or-call-macro
       )
      (:prefix ("r" . "Ring")
       :desc "Display ring head" "L"            #'kmacro-view-ring-2nd
       :desc "Delete ring head" "d"             #'kmacro-delete-ring-head
       :desc "Run 2nd macro in ring" "l"        #'kmacro-call-ring-2nd-repeat
       :desc "Next in ring" "n"                 #'kmacro-cycle-ring-next
       :desc "Previous in ring" "p"             #'kmacro-cycle-ring-previous
       :desc "Swap first two" "s"               #'kmacro-swap-ring
       :desc "View last macro" "v"              #'kmacro-view-macro-repeat
       )
      )
;;; <leader> n --- notes
(map! :leader
      :prefix ("n" . "notes")
      :desc "Active org-clock"               "o" #'org-clock-goto
      :desc "Browse notes"                   "F" #'+default/browse-notes
      :desc "Cancel current org-clock"       "C" #'org-clock-cancel
      :desc "Find file in notes"             "f" #'+default/find-in-notes
      :desc "Goto capture"                   "N" #'org-capture-goto-target
      :desc "Open deft"                      "d" #'deft
      :desc "Org agenda"                     "a" #'org-agenda
      :desc "Org capture"                    "n" #'org-capture
      :desc "Org export to clipboard as RTF" "Y" #'+org/export-to-clipboard-as-rich-text
      :desc "Org export to clipboard"        "y" #'+org/export-to-clipboard
      :desc "Org store link"                 "l" #'org-store-link
      :desc "Search notes for symbol"        "." #'+default/search-notes-for-symbol-at-point
      :desc "Search notes"                   "s" #'+default/org-notes-search
      :desc "Search org agenda headlines"    "S" #'+default/org-notes-headlines
      :desc "Tags search"                    "m" #'org-tags-view
      :desc "Todo list"                      "t" #'org-todo-list
      :desc "Toggle last org-clock"          "c" #'+org/toggle-last-clock
      :desc "View search"                    "v" #'org-search-view

      (:when (featurep! :lang org +journal)
       (:prefix ("j" . "journal")
        :desc "New Entry"      "j" #'org-journal-new-entry
        :desc "Search Forever" "s" #'org-journal-search-forever))
      (:when (featurep! :lang org +roam)
       (:prefix ("r" . "roam")
        :desc "Find file"                     "f" #'org-roam-find-file
        :desc "Insert (skipping org-capture)" "I" #'org-roam-insert-immediate
        :desc "Insert"                        "i" #'org-roam-insert
        :desc "Org Roam Capture"              "c" #'org-roam-capture
        :desc "Org Roam"                      "r" #'org-roam
        :desc "Show graph"                    "g" #'org-roam-graph
        :desc "Switch to buffer"              "b" #'org-roam-switch-to-buffer
        (:prefix ("d" . "by date")
         :desc "Arbitrary date" "d" #'org-roam-dailies-date
         :desc "Today"          "t" #'org-roam-dailies-today
         :desc "Tomorrow"       "m" #'org-roam-dailies-tomorrow
         :desc "Yesterday"      "y" #'org-roam-dailies-yesterday)))
      (:when (featurep! :tools biblio)
       :desc "Bibliographic entries"        "b"
       (cond ((featurep! :completion ivy)   #'ivy-bibtex)
             ((featurep! :completion helm)  #'helm-bibtex)))
      (:when (featurep! :lang org +noter)
       :desc "Org noter"                  "e" #'org-noter)
      )
;;; <leader> o --- open
(map! :leader
      "o" nil ; we need to unbind it first as Org claims this prefix
      :prefix ("o" . "open")
      :desc "Default browser"    "b"             #'browse-url-of-file
      :desc "Debugger"           "d"             #'+debugger/start
      :desc "Dired"              "-"             #'dired-jump
      :desc "Org agenda"         "A"             #'org-agenda
      :desc "REPL (same window)" "R"             #'+eval/open-repl-same-window
      :desc "REPL"               "r"             #'+eval/open-repl-other-window

      (:when (featurep! :ui neotree)
       :desc "Project sidebar"               "p" #'+neotree/open
       :desc "Find file in project sidebar"  "P" #'+neotree/find-this-file)
      (:when (featurep! :ui treemacs)
       :desc "Project sidebar"               "p" #'+treemacs/toggle
       :desc "Find file in project rsidebar" "P" #'treemacs-find-file)
      (:when (featurep! :term shell)
       :desc "Toggle shell popup"            "t" #'+shell/toggle
       :desc "Open shell here"               "T" #'+shell/here)
      (:when (featurep! :term term)
       :desc "Toggle terminal popup"         "t" #'+term/toggle
       :desc "Open terminal here"            "T" #'+term/here)
      (:when (featurep! :os macos)
       :desc "Reveal in Finder"           "f"    #'+macos/reveal-in-finder
       :desc "Reveal project in Finder"   "F"    #'+macos/reveal-project-in-finder
       :desc "Send to Transmit"           "u"    #'+macos/send-to-transmit
       :desc "Send project to Transmit"   "U"    #'+macos/send-project-to-transmit
       :desc "Send to Launchbar"          "l"    #'+macos/send-to-launchbar
       :desc "Send project to Launchbar"  "L"    #'+macos/send-project-to-launchbar
       :desc "Open in iTerm"              "i"    #'+macos/open-in-iterm)
      (:prefix ("a" . "org agenda")
       :desc "Agenda"         "a"                #'org-agenda
       :desc "Todo list"      "t"                #'org-todo-list
       :desc "Tags search"    "m"                #'org-tags-view
       :desc "View search"    "v"                #'org-search-view)
      )
;;; <leader> p --- project
(map! :leader
      :prefix ("p" . "project")
      :desc "Root Shell"                   "'"  #'projectile-run-shell
      :desc "Add new project"              "a"  #'projectile-add-known-project
      :desc "Browse other project"         ">"  #'doom/browse-in-other-project
      :desc "Browse project"               "b"  #'+default/browse-project
      :desc "Compile in project"           "c"  #'projectile-compile-project
      :desc "Configure project"            "g"  #'projectile-configure-project
      :desc "Discover projects in folder"  "d"  #'+default/discover-projects
      :desc "Edit project .dir-locals"     "e"  #'projectile-edit-dir-locals
      :desc "Find file in project"         "f"  #'projectile-find-file
      :desc "Find other file"              "o"  #'projectile-find-other-file
      :desc "Find recent project files"    "r"  #'projectile-recentf
      :desc "Invalidate project cache"     "I"  #'projectile-invalidate-cache
      :desc "Kill project buffers"         "K"  #'projectile-kill-buffers
      :desc "List project todos"          "t"   #'magit-todos-list
      :desc "Open project scratch buffer" "x"   #'doom/open-project-scratch-buffer
      :desc "Remove known project"         "D"  #'projectile-remove-known-project
      :desc "Repeat last command"          "C"  #'projectile-repeat-last-command
      :desc "Run cmd in project root"      "!"  #'projectile-run-shell-command-in-root
      :desc "Run project"                  "R"  #'projectile-run-project
      :desc "Save project files"           "S"  #'projectile-save-project-buffers
      :desc "Search project for symbol"     "." #'+default/search-project-for-symbol-at-point
      :desc "Search project"                "s" #'+default/search-project
      :desc "Switch project"               "p"  #'projectile-switch-project
      :desc "Switch to project buffer"     "b"  #'projectile-switch-to-buffer
      :desc "Switch to scratch buffer"     "X"  #'doom/switch-to-project-scratch-buffer
      :desc "Test project"                 "T"  #'projectile-test-project

      (:when (and (featurep! :tools taskrunner)
                  (or (featurep! :completion ivy)
                      (featurep! :completion helm)))
       :desc "List project tasks"         "z"   #'+taskrunner/project-tasks)
      ;; later expanded by projectile
      (:prefix ("4" . "in other window"))
      (:prefix ("5" . "in other frame"))
      )
;;; <leader> q --- quit/restart
(map! :leader
      :prefix ("q" . "quit/restart")
      :desc "Clear current frame"          "F" #'doom/kill-all-buffers
      :desc "Delete frame"                 "f" #'delete-frame
      :desc "Kill Emacs (and daemon)"      "K" #'save-buffers-kill-emacs
      :desc "Quick save current session"   "s" #'doom/quicksave-session
      :desc "Quit Emacs"                   "q" #'kill-emacs
      :desc "Restart & restore Emacs"      "r" #'doom/restart-and-restore
      :desc "Restart emacs server"         "d" #'+default/restart-server
      :desc "Restart Emacs"                "R" #'doom/restart
      :desc "Restore last session"         "l" #'doom/quickload-session
      :desc "Restore session from file"    "L" #'doom/load-session
      :desc "Save session to file"         "S" #'doom/save-session
      )
;;; <leader> r -- REGISTERS
(map! :leader
      :prefix ("r" . "Registers")
      :desc "Insert Register"      "i" #'insert-register
      :desc "Save to Register"     "x" #'copy-to-register
      :desc "Windows to Register"  "w" #'window-configuration-to-register
      :desc "Jump to Register"     "j" #'jump-to-register
      :desc "List Registers"       "l" #'list-registers
      :desc "Killed Text"          "y" #'counsel-yank-pop
      )
;;; <leader> R --- remote
(map! :leader
      (:when (featurep! :tools upload)
       :prefix ("R" . "remote")
       :desc "Browse relative"            "B" #'ssh-deploy-browse-remote-handler
       :desc "Browse remote files"        "." #'ssh-deploy-browse-remote-handler
       :desc "Browse remote"              "b" #'ssh-deploy-browse-remote-base-handler
       :desc "Delete local & remote"      "D" #'ssh-deploy-delete-handler
       :desc "Detect remote changes"      ">" #'ssh-deploy-remote-changes-handler
       :desc "Diff local & remote"        "x" #'ssh-deploy-diff-handler
       :desc "Download remote"            "d" #'ssh-deploy-download-handler
       :desc "Eshell base terminal"       "e" #'ssh-deploy-remote-terminal-eshell-base-handler
       :desc "Eshell relative terminal"   "E" #'ssh-deploy-remote-terminal-eshell-handler
       :desc "Move/rename local & remote" "m" #'ssh-deploy-rename-handler
       :desc "Open this file on remote"   "o" #'ssh-deploy-open-remote-file-handler
       :desc "Run deploy script"          "s" #'ssh-deploy-run-deploy-script-handler
       :desc "Upload local (force)"       "U" #'ssh-deploy-upload-handler-forced
       :desc "Upload local"               "u" #'ssh-deploy-upload-handler
       )
      )
;;; <leader> s --- search
(map! :leader
      :prefix ("s" . "search")
      :desc "Search Clear" "c"                 #'evil-ex-nohighlight
      :desc "Dictionary"                   "t" #'+lookup/dictionary-definition
      :desc "Jump list"                    "j" #'evil-show-jumps
      :desc "Jump to bookmark"             "b" #'bookmark-jump
      :desc "Jump to link"                 "L" #'ffap-menu
      :desc "Jump to mark"                 "m" #'evil-show-marks
      :desc "Jump to symbol"               "i" #'imenu
      :desc "Jump to visible link"         "l" #'link-hint-open-link
      :desc "Google"                       "g" #'jg-browse-url
      :desc "Locate file"                  "f" #'+lookup/file
      :desc "Locate file"                  "f" #'locate
      :desc "Look up in all docsets"       "K" #'+lookup/in-all-docsets
      :desc "Look up in local docsets"     "k" #'+lookup/in-docsets
      :desc "Look up online (w/ prompt)"   "O" #'+lookup/online-select
      :desc "Look up online"               "o" #'+lookup/online
      :desc "Search buffer"                "s" #'swiper
      :desc "Search buffer"                "S" #'+default/search-buffer
      :desc "Search current directory"     "d" #'+default/search-cwd
      :desc "Search other directory"       "D" #'+default/search-other-cwd
      :desc "Search project"               "p" #'+default/search-project
      :desc "Search other project"         "P" #'+default/search-other-project
      :desc "Search project for symbol"    "." #'+default/search-project-for-symbol-at-point
      :desc "Thesaurus"                    "T" #'+lookup/synonyms
      )
;;; <leader> t --- toggle
(map! :leader
      :prefix ("t" . "toggle")
       "d" nil
       "v" nil
       "n" nil
       "C"                        #'global-company-mode
       :desc "Input Language" "i" #'toggle-input-method
       :desc "SmartParens"    "s" #'smartparens-global-mode
       (:prefix ("d" . "Debug")
        :desc "Debug on Error" "e"               #'toggle-debug-on-error
        :desc "Debug on Var" "v"                 #'debug-on-variable-change
        :desc "Cancel Debug on Var" "V"          #'cancel-debug-on-variable-change
        :desc "Debug on Function" "f"            #'debug-on-entry
        :desc "Cancel Debug on Function" "F"     #'cancel-debug-on-entry
        :desc "Flymake"                      "C" #'flymake-mode
        (:when (featurep! :checkers syntax)
         :desc "Flycheck"                   "c"  #'global-flycheck-mode)
        (:when (and (featurep! :checkers spell) (not (featurep! :checkers spell +flyspell)))
         :desc "Spell checker"              "s"  #'spell-fu-mode)
        (:when (featurep! :checkers spell +flyspell)
         :desc "Spell checker"              "s"  #'flyspell-mode))
       (:prefix ("v" . "Visual")
        :desc "Big mode"                     "b" #'doom-big-font-mode
        :desc "Frame fullscreen"             "F" #'toggle-frame-fullscreen
        :desc "Line numbers"                 "n" #'+jg-toggle-line-numbers
        :desc "Line numbers Visual"          "N" #'+jg-toggle-line-numbers-visual
        :desc "Evil goggles"                 "g" #'evil-goggles-mode
        :desc "Soft line wrapping"           "w" #'visual-line-mode
        :desc "Fill Column Indicator"        "f" #'display-fill-column-indicator-mode
        :desc "Ignore Invisible"             "i" #'+jg-toggle-line-move-ignore-invisible
        (:when (featurep! :editor word-wrap)
         :desc "Soft line wrapping"         "w"  #'+word-wrap-mode)
        :desc "Word-wrap mode"               "W" #'+word-wrap-mode
        :desc "Whitespace"                  "w"  #'whitespace-mode
        :desc "Line Truncate"               "l"  #'toggle-truncate-lines
        )
       (:prefix  ("n" . "Navigation")
        :desc "Neotree"                      "t" #'neotree-toggle
        (:when (featurep! :ui minimap)
         :desc "Minimap mode"               "m"  #'minimap-mode)
        (:when (featurep! :lang org +present)
         :desc "org-tree-slide mode"        "p"  #'org-tree-slide-mode)

        :desc "Centered Cursor"              "c" #'centered-cursor-mode
        (:when (featurep! :ui indent-guides)
         :desc "Indent guides"              "i"  #'highlight-indent-guides-mode)
        :desc "Indent style"                 "I" #'doom/toggle-indent-style
        :desc "Evil-visual-mark"             "v"  #'evil-visual-mark-mode
        :desc "Auto-Highlight"               "h"  #'auto-highlight-symbol-mode
        )
       (:when (featurep! :lang org +pomodoro)
        :desc "Pomodoro timer"             "t"   #'org-pomodoro)
       ;; highlight long lines
       ;; auto-completion
       ;; camel-case-motion
       ;; fill-column indicator
      )

(map! :leader
      :when (featurep! :ui workspaces)
      :prefix ("W" . "Workspaces")
      :desc "Workspace Counsel" "RET"            #'jg-counsel-workspace
      ;; Lowercase - workspace, Uppercase - session
      :desc "Display tab bar"              "TAB" #'+workspace/display
      :desc "Next workspace"               "]"   #'+workspace/switch-right
      :desc "Previous workspace"           "["   #'+workspace/switch-left
      :desc "Switch to last workspace"     "`"   #'+workspace/other
      :desc "Switch workspace"             "."   #'+workspace/switch-to
      :desc "Switch to last workspace"     "0"   #'+workspace/switch-to-final

      :desc "Create workspace"             "c"   #'+workspace/new
      :desc "Delete workspace"             "k"   #'+workspace/delete
      :desc "Load workspace from file"     "l"   #'+workspace/load
      :desc "New workspace"                "n"   #'+workspace/new
      :desc "Rename workspace"             "r"   #'+workspace/rename
      :desc "Save workspace"               "s"   #'+workspace/save
      :desc "Delete session"               "x"   #'+workspace/kill-session

      :desc "Autosave session"              "A"  #'doom/quicksave-session
      :desc "Load session"                  "L"  #'doom/load-session
      :desc "Redo window config"            "U"  #'winner-redo
      :desc "Save session"                  "S"  #'doom/save-session
      :desc "Undo window config"            "u"  #'winner-undo
      )
;;; <leader> W --- workspaces
(map! :leader
      :prefix ("w" . "Windows")
      :desc "Toggle Dedicated" "DEL"                #'+jg-toggle-window-dedication
      :desc "Delete Window" "d"                   #'delete-window
      :desc "Split To Right" "/"                  #'split-window-right
      :desc "Split Below" "-"                     #'split-window-below
      "k"                                         #'evil-window-up
      "l"                                         #'evil-window-right
      "h"                                         #'evil-window-left
      "j"                                         #'evil-window-down
      "m"                                         #'doom/window-enlargen
      "M"                                         #'doom/window-maximize-buffer
      "b"                                         #'balance-windows

      "{"                                         #'shrink-window-horizontally
      "}"                                         #'shrink-window
      )
;;; <leader> x -- Text
(map! :leader
      :prefix ("x" . "Text")
      :desc "Mark" "m"                                    #'mark-whole-buffer
      :desc "Iedit" "e"                                   #'evil-iedit-state/iedit-mode
      :desc "Regexp Builder" "r"                          #'regexp-builder
      ;; align
      (:prefix ("a" . "Align")
       :desc "Regexp"    "r"                              #'align-regexp
       )
      ;; Lines
      (:prefix ("l" . "Lines")
       ;; untabify
       :desc "Flush Lines" "f"                                    #'flush-lines
       :desc "Keep Lines"  "K"                                    #'keep-lines
       :desc "Delete trailing newlines"   "W" #'doom/delete-trailing-newlines
       :desc "Delete trailing whitespace" "w" #'delete-trailing-whitespace
       :desc "Whitespace Cleanup"         "c" #'whitespace-cleanup
       :desc "Indent"                     "i" #'indent-region
       :desc "Random"                     "r" #'+jg-bindings-goto-random-line
       )
      (:prefix ("i" . "insert"))
      )
;;; <leader> y --- snippets
(map! :leader
      (:when (featurep! :editor snippets)
       :prefix ("y" . "snippets")
       :desc "Expand Snippet"        "y" #'yas-expand
       :desc "New snippet"           "n" #'yas-new-snippet
       :desc "Insert snippet"        "i" #'yas-insert-snippet
       :desc "Find global snippet"   "/" #'yas-visit-snippet-file
       :desc "Reload snippets"       "r" #'yas-reload-all
       :desc "Create Temp Template"  "c" #'aya-create
       :desc "Use Temp Template"     "e" #'aya-expand)
      )

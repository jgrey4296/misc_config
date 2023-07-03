;; Leader no prefix
;;
(doom-log "Setting up leader bindings: %s" (current-time-string))
;;-- leader
(map! :leader
      ;; "," "<" "!" "?"
      :desc "Ibuffer"               "DEL" #'ibuffer
      :desc "Jump to bookmark"      "RET" #'bookmark-jump

      :desc "Find file"             "."   #'find-file

      :desc "Pop Shell"             "'"   #'shell
      :desc "Switch to last buffer" "TAB" #'evil-switch-to-windows-last-buffer
      :desc "Split Window"          "/"   #'split-window-right
      :desc "Toggle last popup"     "`"   #'+popup/toggle

      :desc "Eval expression"       "\""   #'pp-eval-expression
      :desc "M-x"                   ";"   #'execute-extended-command

      :desc "Org Capture"           "X"   #'org-capture

      ;; C-u is used by evil
      :desc "Universal argument"    "u"   #'universal-argument

      ;; 0 6 9 8
      :desc "Agenda"    "7" (cmd! (find-file (expand-file-name "base_agenda.org" doom-user-dir)))
      )

(map! :leader
      (:prefix ("y" . "snippets"))
      (:prefix ("B" . "Bookmarks"))
      (:prefix ("p". "Project"))
      (:prefix ("m" . "Local Mode"))
      )
;;-- end leader

;;-- <leader> b --- buffer
(map! :leader
      :prefix ("b" . "buffer")
      ;; DEL
      ;; -
      ;; n
      :desc "Ediff Buffers"               "TAB" #'ediff-buffers
      :desc "Next buffer"                 "]"   #'next-buffer
      :desc "Previous buffer"             "["   #'previous-buffer

      :desc "Switch buffer"               "b"   #'switch-to-buffer
      :desc "Create Buffer"               "c"   #'evil-buffer-new
      :desc "Kill buffer"                 "d"   #'kill-current-buffer
      :desc "Change Extension"            "e"   #'+jg-binding-change-ext
      :desc "Clone Indirect"              "i"   #'clone-indirect-buffer-other-window
      :desc "Kill all buffers"            "K"   #'doom/kill-all-buffers
      ;; :desc "Local Variables"             "l"   #'+jg-bindings-list-buffer-locals

      :desc "Kill other buffers"          "O"   #'doom/kill-other-buffers
      :desc "Read-only mode"              "r"   #'read-only-mode
      :desc "Revert buffer"               "R"   #'revert-buffer
      :desc "Save all buffers"            "S"   #'evil-write-all
      :desc "Save buffer"                 "s"   #'basic-save-buffer
      :desc "Pop up scratch buffer"       "x"   #'doom/open-scratch-buffer
      :desc "Bury buffer"                 "z"   #'bury-buffer

      )
;;-- end <leader> b --- buffer

;;-- <leader> c --- code
(map! :leader
      :prefix ("c" . "code")
      :desc "Compile"                               "c"   #'compile
      :desc "Recompile"                             "C"   #'recompile
      :desc "Evaluate & replace region"             "E"   #'+eval:replace-region
      :desc "Evaluate buffer/region"                "e"   #'+eval/buffer-or-region
      :desc "List errors"                           "x"   #'flymake-show-diagnostics-buffer
      :desc "Recompile"                             "C"   #'recompile
      :desc "Send to repl"                          "s"   #'+jg-send-region-to-repl
      :desc "List errors"                           "x"   #'flycheck-list-errors
      (:prefix ("r" . "Repl"))
      )
;;-- end <leader> c --- code

;;-- <leader> f --- file
(map! :leader
      :prefix ("f" . "file")
      :desc "Ediff Files"                "TAB" #'ediff-files

      :desc "Browse emacs.d"              "E"   #'+default/browse-emacsd
      :desc "Browse private config"       "P"   #'doom/open-private-config
      :desc "Copy this file"              "C"   #'doom/copy-this-file
      :desc "Delete this file"            "D"   #'doom/delete-this-file
      :desc "Find file from here"         "F"   #'+default/find-file-under-here
      :desc "Find file in emacs.d"        "e"   #'+default/find-in-emacsd
      :desc "Find file in private config" "p"   #'doom/open-private-config
      :desc "Find file"                   "f"   #'find-file
      :desc "Locate file"                 "l"   #'locate
      :desc "Recent files"                "r"   #'recentf-open-files
      :desc "Rename/move file"            "M"   #'doom/move-this-file
      :desc "Save file as..."             "S"   #'write-file
      :desc "Save file"                   "s"   #'save-buffer
      :desc "Sudo find file"              "U"   #'doom/sudo-find-file
      :desc "Yank filename"               "n"   #'+default/yank-buffer-path

      )
;;-- end <leader> f --- file

;;-- <leader> g --- git
(map! :leader
      :prefix ("g" . "git")
      ;; 1
      :desc "Git revert file"             "R"   #'vc-revert
       :desc "Git revert hunk"            "r"   #'git-gutter:revert-hunk
       :desc "Git stage hunk"             "s"   #'git-gutter:stage-hunk
       :desc "Git time machine"           "t"   #'git-timemachine-toggle
       :desc "Jump to next hunk"          "n"   #'git-gutter:next-hunk
       :desc "Jump to previous hunk"      "p"   #'git-gutter:previous-hunk
       :desc "Forge dispatch"             "'"   #'forge-dispatch
       :desc "Git stage file"            "S"   #'magit-stage-file
       :desc "Git unstage file"          "U"   #'magit-unstage-file
       :desc "Magit blame"                "B"   #'magit-blame-addition
       :desc "Magit buffer log"           "L"   #'magit-log
       :desc "Magit clone"               "C"   #'magit-clone
       :desc "Magit dispatch"             "/"   #'magit-dispatch
       :desc "Magit fetch"                "F"   #'magit-fetch
       :desc "Magit file delete"         "D"   #'magit-file-delete
       :desc "Magit file dispatch"       "."   #'magit-file-dispatch
       :desc "Magit status here"          "S"   #'magit-status-here
       :desc "Magit status"               "s"   #'magit-status
       :desc "Magit switch branch"       "b"   #'magit-branch-checkout
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
        :desc "List repositories"         "r"   #'magit-list-repositories
        :desc "List submodules"           "s"   #'magit-list-submodules
        :desc "List issues"               "i"   #'forge-list-issues
        :desc "List pull requests"        "p"   #'forge-list-pullreqs
        :desc "List notifications"        "n"   #'forge-list-notifications)
       (:prefix ("c" . "create")
        :desc "Branch"                    "b"   #'magit-branch-and-checkout
        :desc "Initialize repo"           "r"   #'magit-init
        :desc "Clone repo"                "R"   #'magit-clone
        :desc "Commit"                    "c"   #'magit-commit-create
        :desc "Fixup"                     "f"   #'magit-commit-fixup
        :desc "Issue"                     "i"   #'forge-create-issue
        :desc "Pull request"              "p"   #'forge-create-pullreq)
      )
;;-- end <leader> g --- git

;; -- <leader> h - help

;;-- <leader> i --- insert
(map! :leader
      :prefix ("i" . "insert")
      ;; d
      :desc "Evil ex path"                  ":"   (cmd! (evil-ex "R!echo "))
      :desc "From clipboard"                "y"   #'+default/yank-pop
      :desc "From evil register"            "r"   #'counsel-evil-registers
      :desc "From Minibuffer history"       "m"   #'counsel-minibuffer-history
      :desc "Unicode"                       "u"   #'insert-char
      :desc "Snippet"                       "s"   #'yas-insert-snippet

      )
;;-- end <leader> i --- insert

;;-- <leader> j -- Jumping
(map! :leader
      :desc "Jump"    "j" #'jg-binding-jump-map)
;;-- end <leader> j -- Jumping

;;-- <leader> M -- Macros
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
;;-- end <leader> M -- Macros

;;-- <leader> n --- notes
(map! :leader
      :prefix ("n" . "notes")
      :desc "Org noter"                      "e" #'org-noter
      :desc "Active org-clock"               "o" #'org-clock-goto
      :desc "Cancel current org-clock"       "C" #'org-clock-cancel
      :desc "Goto capture"                   "N" #'org-capture-goto-target
      :desc "Open deft"                      "d" #'deft
      :desc "Org export to clipboard as RTF" "Y" #'+org/export-to-clipboard-as-rich-text
      :desc "Org export to clipboard"        "y" #'+org/export-to-clipboard
      :desc "Org store link"                 "l" #'org-store-link
      :desc "Search notes for symbol"        "." #'+default/search-notes-for-symbol-at-point
      :desc "Search notes"                   "s" #'+default/org-notes-search
      :desc "Search org agenda headlines"    "S" #'+default/org-notes-headlines
      :desc "Tags search"                    "m" #'org-tags-view
      :desc "Toggle last org-clock"          "c" #'+org/toggle-last-clock
      :desc "View search"                    "v" #'org-search-view
      (:prefix ("j" . "journal")
       :desc "New Entry"      "j" #'org-journal-new-entry
       :desc "New Scheduled Entry" "J" #'org-journal-new-scheduled-entry
       :desc "Search Forever" "s" #'org-journal-search-forever)
      )
;;-- end <leader> n --- notes

;;-- <leader> o --- open
(map! :leader
      :prefix ("o" . "open")
      :desc "Command History"              "DEL"  #'counsel-command-history
      :desc "Minibuffer history"           "0"    #'counsel-minibuffer-history

      :desc "Default browser"              "b"    #'browse-url-of-file
      :desc "Calc"                         "C"    #'calc
      :desc "Dired"                        "d"    #'dired-jump
      ;; :desc "Compose Email"                "e" #'mu4e~compose-mail
      :desc "REPL"                         "r"    #'+eval/open-repl-other-window
      :desc "Regexp Builder"               "R"    #'regexp-builder
      :desc "Repo Homepage"                "g"    #'+vc/browse-at-remote-homepage
      :desc "Github Homepage"              "G"    (cmd! (browse-url user-url))

      :desc "Rmail"                        "m"    #'rmail
      :desc "External Mail"                "M"    #'mu4e
      :desc "Browse notes"                 "n"    #'+default/browse-notes
      :desc "Notes"                        "N"    #'+default/find-in-notes

      :desc "Todo list"                    "t"    #'org-todo-list

      (:when (modulep! :os macos)
       :desc "Reveal in Finder"           "f"     #'+macos/reveal-in-finder
       )

      (:prefix ("a" . "org agenda"))

      (:prefix ("s" . "Systems"))
      )
;;-- end <leader> o --- open

;;-- <leader> q --- quit/restart/session
(map! :leader
      :prefix ("q" . "quit/restart")
      :desc "Quit Emacs"                   "q" #'kill-emacs
      :desc "Pause Emacs"                  "p" #'suspend-emacs
      :desc "Kill Server"                  "s" (cmd! (server-force-stop))
      :desc "Restart emacs server"         "S" #'+default/restart-server

       (:prefix ("f" . "Frames")
        :desc "Clear current frame"          "F" #'doom/kill-all-buffers
        :desc "Delete frame"                 "f" #'delete-frame
        )
       (:prefix ("w" . "Sessions")
        :desc "Save current session"         "s" #'doom/quicksave-session
        :desc "Load last session"            "l" #'doom/quickload-session

        :desc "Load session from file"       "L" #'doom/load-session
        :desc "Save session to file"         "S" #'doom/save-session
        )
      )
;;-- end <leader> q --- quit/restart/session

;;-- <leader> r -- REGISTERS
(map! :leader
      :prefix ("r" . "Registers")
      :desc "Insert Register"      "i" #'insert-register
      :desc "Save to Register"     "x" #'copy-to-register
      :desc "Windows to Register"  "w" #'window-configuration-to-register
      :desc "Jump to Register"     "j" #'jump-to-register
      :desc "List Registers"       "l" #'list-registers
      :desc "Killed Text"          "y" #'counsel-yank-pop
      )
;;-- end <leader> r -- REGISTERS

;;-- <leader> R --- remote
(map! :leader
      :prefix "r"

      )
;;-- end <leader> R --- remote

;;-- <leader> s --- search
(map! :leader
      :prefix ("s" . "search")
      :desc "Search Clear"                 "c" #'evil-ex-nohighlight
      :desc "Dictionary"                   "t" #'+lookup/dictionary-definition
      :desc "Jump list"                    "j" #'evil-show-jumps
      :desc "Jump to link"                 "L" #'ffap-menu
      :desc "Jump to mark"                 "m" #'evil-show-marks
      :desc "Jump to symbol"               "i" #'imenu
      :desc "Jump to visible link"         "l" #'link-hint-open-link
      :desc "Google"                       "g" #'browse-url
      :desc "Look up in all docsets"       "K" #'+lookup/in-all-docsets
      :desc "Look up in local docsets"     "k" #'+lookup/in-docsets
      :desc "Look up online (w/ prompt)"   "O" #'+lookup/online-select
      :desc "Look up online"               "o" #'+lookup/online
      :desc "Search buffer"                "S" #'+default/search-buffer
      :desc "Search current directory"     "d" #'+default/search-cwd
      :desc "Search other directory"       "D" #'+default/search-other-cwd
      :desc "Search project"               "p" #'+default/search-project
      :desc "Search other project"         "P" #'+default/search-other-project
      :desc "Search project for symbol"    "." #'+default/search-project-for-symbol-at-point
      :desc "Dictionary"                   "t" #'+lookup/dictionary-definition
      :desc "Thesaurus"                    "T" #'+lookup/synonyms
      )
;;-- end <leader> s --- search

;;-- <leader> t --- toggle
(map! :leader
      :prefix ("t" . "toggle")
      (:prefix ("i" . "Input")
       :desc "Global Company" "C" #'global-company-mode
       :desc "Input Language" "i" #'toggle-input-method
       :desc "SmartParens"    "s" #'smartparens-global-mode
       :desc "Read-only mode" "r" #'read-only-mode
       :desc "Indent style"   "I" #'doom/toggle-indent-style
       )
      ;; v - visual toggles set as hydra in jg-ui
      (:prefix ("d" . "Debug")
       :desc "Debug on Error"            "e" #'toggle-debug-on-error
       :desc "Debug on Var"              "v" #'debug-on-variable-change
       :desc "Cancel Debug on Var"       "V" #'cancel-debug-on-variable-change
       :desc "Debug on Function"         "f" #'debug-on-entry
       :desc "Cancel Debug on Function"  "F" #'cancel-debug-on-entry
       :desc "Flymake"                   "C" #'flymake-mode
       :desc "Flycheck"                  "c" #'global-flycheck-mode
       :desc "Spell checker"            "s" #'flyspell-mode
       )

      (:when (modulep! :lang org +pomodoro)
       :desc "Pomodoro timer"             "t"   #'org-pomodoro)
      ;; highlight long lines
      ;; camel-case-motion
      )
;;-- end <leader> t --- toggle

;;-- <leader> w --- Windows
(map! :leader
      :prefix ("w" . "Windows")
      ;; RET - workspace counsel
      :desc "Balance"             "b"   #'balance-windows
      :desc "Delete Window"       "d"   #'delete-window
      :desc "Maximize"            "m"   #'doom/window-maximize-buffer
      :desc "Undo window config"  "u"   #'winner-undo
      :desc "Redo window config"  "U"   #'winner-redo
      :desc "Window left"         "h"   #'evil-window-left
      :desc "Window right"        "j"   #'evil-window-down
      :desc "Window right"        "l"   #'evil-window-right
      :desc "Window up"           "k"   #'evil-window-up

      :desc "Split Below"         "-"   #'split-window-below
      :desc "Split To Right"      "/"   #'split-window-right
      :desc "Shrink Horizontal"   "{"   #'shrink-window-horizontally
      :desc "Shrink Vertical"     "}"   #'shrink-window

      )
;;-- end <leader> w --- Windows

;;-- <leader> x -- Text
(map! :leader
      :prefix ("x" . "Text")
      )
;;-- end <leader> x -- Text

(provide 'jg-leader-bindings-loaded)

;; Leader no prefix
;;
(message "Setting up leader bindings: %s" (current-time-string))
;;-- leader
(map! :leader
      ;; "," "<" "!" "?"
      ;; :desc "help"                  "h"    help-map
      :desc "Goto-line"             "SPC" #'evil-avy-goto-line
      :desc "Ibuffer"               "DEL" #'ibuffer
      :desc "Jump to bookmark"      "RET" #'bookmark-jump

      :desc "Find file"             "."   #'find-file


      :desc "Pop Shell"             "'"   #'shell
      :desc "Switch to last buffer" "TAB" #'evil-switch-to-windows-last-buffer
      :desc "Split Window"          "/"   #'split-window-right
      :desc "Toggle last popup"     "`"   #'+popup/toggle

      :desc "Evaluate line/region"  "e"   #'eval-last-sexp

      :desc "Eval expression"       "\""   #'pp-eval-expression
      :desc "M-x"                   ";"   #'execute-extended-command

      :desc "Org Capture"           "X"   #'org-capture

      ;; C-u is used by evil
      :desc "Universal argument"    "u"   #'universal-argument

      ;; :desc "Resume last search"    "'"
      ;; #'ivy-resume
      ;; #'helm-resume
      ;; 0 6 9 8
      :desc "Desktop"   "1" (cmd! (find-file "~/Desktop"))
      :desc "Github"    "2" (cmd! (find-file "~/github"))
      :desc "Mega"      "3" (cmd! (find-file "~/mega"))
      :desc "Home"      "4" (cmd! (find-file "~"))
      :desc "Resources" "5" (cmd! (find-file "~/github/writing/resources"))
      :desc "Agenda"    "7" (cmd! (find-file (car org-agenda-files)))
      )

;;-- end leader

;;-- <leader> a -- Unused
(map! :leader
      :prefix "a"
      )
;;-- end <leader> a -- Unused

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
      :desc "Clone Indirect"              "i"   #'clone-indirect-buffer-other-window
      :desc "Kill all buffers"            "K"   #'doom/kill-all-buffers
      :desc "Local Variables"             "l"   #'+jg-bindings-list-buffer-locals

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
      :desc "Find implementations"                  "i"   #'+lookup/implementations
      :desc "Find type definition"                  "t"   #'+lookup/type-definition
      :desc "Format buffer/region"                  "f"   #'+format/region-or-buffer
      :desc "List errors"                           "x"   #'flymake-show-diagnostics-buffer
      :desc "Recompile"                             "C"   #'recompile
      :desc "Send to repl"                          "s"   #'+jg-send-region-to-repl
       ;;:desc "Flycheck"                           "!"   flycheck-command-map
      :desc "List errors"                           "x"   #'flycheck-list-errors
      :desc "LSP Code actions"                      "a"   #'lsp-execute-code-action
      :desc "LSP Organize imports"                  "o"   #'lsp-organize-imports
      :desc "LSP Rename"                            "R"   #'lsp-rename
      :desc "LSP"                                   "l"   #'lsp-command-map
      :desc "Jump to symbol in current workspace"   "j"   #'lsp-ivy-workspace-symbol
      :desc "Jump to symbol in any workspace"       "J"   #'lsp-ivy-global-workspace-symbol
      :desc "Macro Expand"                          "m"   #'pp-macroexpand-last-sexp
      (:prefix ("r" . "Repl")
       :desc "Clear" "c" #'+jg-repl-clear

       )
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
      :desc "Open project editorconfig"   "c"   #'editorconfig-find-current-editorconfig
      :desc "Recent files"                "r"   #'recentf-open-files
      :desc "Recent project files"        "R"   #'projectile-recentf
      :desc "Rename/move file"            "M"   #'doom/move-this-file
      :desc "Save file as..."             "S"   #'write-file
      :desc "Save file"                   "s"   #'save-buffer
      :desc "Sudo find file"              "U"   #'doom/sudo-find-file
      :desc "Yank filename"               "n"   #'+default/yank-buffer-path

      (:prefix ("b" . "Bookmark")
       :desc "Set bookmark"                "m" #'bookmark-set
       :desc "Delete bookmark"             "M" #'bookmark-delete
       :desc "Rename bookmark"             "r" #'bookmark-rename
       :desc "Save Bookmarks"              "s" #'bookmark-save
       :desc "Load Bookmarks"              "l" #'bookmark-load
       )
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

;;-- <leader> i --- insert
(map! :leader
      :prefix ("i" . "insert")
      ;; d
      :desc "Current file name"             "f"   #'+default/insert-file-path
      :desc "Current file path"             "F"   (cmd!! #'+default/insert-file-path t)
      :desc "Evil ex path"                  "p"   (cmd! (evil-ex "R!echo "))
      :desc "From clipboard"                "y"   #'+default/yank-pop
      :desc "From evil register"            "r"   #'counsel-evil-registers
      :desc "From Minibuffer history"       "m"   #'counsel-minibuffer-history
      :desc "Unicode"                       "u"    #'insert-char
      ;; TODO date, time

      (:prefix ("L" . "Lorem Ipsum")
       :desc "Sentence"         "s" #'lorem-ipsum-insert-sentences
       :desc "Paragraph"        "p" #'lorem-ipsum-insert-paragraphs
       :desc "List"             "l" #'lorem-ipsum-insert-list
       :desc "Academic"         "a" #'academic-phrases
       :desc "Academic Section" "A" #'academic-phrases-by-section
       )
      ;; TODO password-generator
      ;; TODO uuid
      ;; reserve "d" for inserting debug statement by mode
      :desc "Snippet"                       "s"   #'yas-insert-snippet

      )
;;-- end <leader> i --- insert

;;-- <leader> j -- Jumping
(map! :leader
      :prefix ("j" . "Jump")
      ;; 1 2 3 "u" "h"

      :desc "Parse File"            "!" #'helm-gtags-parse-file
      :desc "Jump to Char"          "." #'avy-goto-char
      :desc "Create Tags"           "C" #'helm-gtags-create-tags
      ;; :desc "Find Tag Other Window" "D" #'helm-gtags-find-tag-other-window
      :desc "References"            "D" #'+lookup/references
      :desc "Update Tags"           "U" #'helm-gtags-update-tags
      ;; :desc "Definition"            "d" #'+lookup/definition
      :desc "Find Tag"              "d" #'helm-gtags-find-tag
      :desc "Implementations"       "i" #'+lookup/implementations
      ;; :desc "Tags in func"          "i" #'helm-gtags-tags-in-this-function
      :desc "Documentation"         "k" #'+lookup/documentation
      :desc "Line"                  "l" #'evil-avy-goto-line
      :desc "Avy Pop Mark"          "m" #'avy-pop-mark
      :desc "Find rtag"             "r" #'helm-gtags-find-rtag
      :desc "Gtags Select"          "s" #'helm-gtags-select
      :desc "Type definition"       "t" #'+lookup/type-definition

      :desc "Find Symbol"           "y" #'helm-gtags-find-symbol
      )
;;-- end <leader> j -- Jumping

;;-- <leader> m -- Local Mode
(map! (:prefix ("m" . "Local Mode")))
;;-- end <leader> m -- Local Mode

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
      :desc "Bibliographic entries"          "b" #'ivy-bibtex
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
      :desc "Calc"                         "c"    #'calc
      :desc "Calendar"                     "C"    #'calendar
      :desc "Dired"                        "d"    #'dired-jump
      ;; :desc "Compose Email"                "e" #'mu4e~compose-mail
      :desc "REPL"                         "r"    #'+eval/open-repl-other-window

      :desc "Project sidebar"              "p"    #'+neotree/open
      :desc "Find file in project sidebar" "P"    #'+neotree/find-this-file

      :desc "Rmail"                        "m"    #'rmail
      :desc "External Mail"                "M"    #'mu4e
      :desc "Browse notes"                 "n"    #'+default/browse-notes
      :desc "Notes"                        "N"    #'+default/find-in-notes

      :desc "Todo list"                    "t"    #'org-todo-list

      (:when (modulep! :os macos)
       :desc "Reveal in Finder"           "f"     #'+macos/reveal-in-finder
       :desc "Reveal project in Finder"   "F"     #'+macos/reveal-project-in-finder
       )

      (:prefix ("a" . "org agenda"))

      (:prefix ("h" . "Helms")
       :desc "Minibuffer History"           "m"   #'counsel-minibuffer-history
       :desc "Shell History"                "s"   #'counsel-shell-history

      )
      (:prefix ("s" . "Systems"))
      )
;;-- end <leader> o --- open

;;-- <leader> p --- project
(map! :leader
      :prefix ("p" . "project")
      :desc "Project Root"                 "`"  (cmd! (find-file (doom-project-root)))
      :desc "Root Shell"                   "'"  #'projectile-run-shell
      :desc "Browse other project"         ">"  #'doom/browse-in-other-project
      :desc "Search project for symbol"     "." #'+default/search-project-for-symbol-at-point

      :desc "Add new project"              "a"  #'projectile-add-known-project
      :desc "Browse project"               "b"  #'+default/browse-project
      :desc "Compile in project"           "c"  #'projectile-compile-project
      :desc "Open project editorconfig"    "C"   #'editorconfig-find-current-editorconfig
      :desc "Configure project"            "g"  #'projectile-configure-project
      :desc "Discover projects in folder"  "d"  #'+default/discover-projects
      :desc "Edit project .dir-locals"     "e"  #'projectile-edit-dir-locals
      :desc "Find file in project"         "f"  #'projectile-find-file
      :desc "Find other file"              "o"  #'projectile-find-other-file
      :desc "Find recent project files"    "r"  #'projectile-recentf
      :desc "Invalidate project cache"     "I"  #'projectile-invalidate-cache
      :desc "Kill project buffers"         "K"  #'projectile-kill-buffers
      :desc "List project todos"           "t"  #'magit-todos-list
      ;; x
      :desc "Remove known project"         "D"  #'projectile-remove-known-project
      :desc "Repeat last command"          "C"  #'projectile-repeat-last-command
      :desc "Run cmd in project root"      "!"  #'projectile-run-shell-command-in-root
      :desc "Run project"                  "R"  #'projectile-run-project
      :desc "Save project files"           "S"  #'projectile-save-project-buffers
      :desc "Search project"               "s"  #'+default/search-project
      :desc "Switch project"               "p"  #'projectile-switch-project
      :desc "Switch to project buffer"     "b"  #'projectile-switch-to-buffer
      :desc "Test project"                 "T"  #'projectile-test-project

      (:prefix ("%" . "Replace")
       :desc "Replace in Project"        "r" #'projectile-replace
       :desc "Replace Regexp in Project" "R" #'projectile-replace-regexp
       )
      ;; later expanded by projectile
      (:prefix ("4" . "in other window"))
      (:prefix ("5" . "in other frame"))
      )
;;-- end <leader> p --- project

;;-- <leader> P -- processes
(map! :leader
      :prefix ("P" . "Processes")
      :desc "List-Processes" "l" (cmd! (list-processes))
      :desc "Helm Processes" "h" #'helm-list-emacs-process
      :desc "Kill Preview"   "p" (cmd! (shell-command "killall Preview"))
      )
;;-- end <leader> P -- processes

;;-- <leader> q --- quit/restart
(map! :leader
      :prefix ("q" . "quit/restart")
      :desc "Quit Emacs"                   "q" #'kill-emacs
      :desc "Pause Emacs"                  "p" #'suspend-emacs
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
;;-- end <leader> q --- quit/restart

;;-- <leader> r -- REGISTERS
(map! :leader
      :prefix ("r" . "Registers")
      :desc "Insert Register"      "i" #'insert-register
      :desc "Save to Register"     "x" #'copy-to-register
      :desc "Windows to Register"  "w" #'window-configuration-to-register
      :desc "Jump to Register"     "j" #'jump-to-register
      :desc "List Registers"       "l" #'list-registers
      :desc "Killed Text"          "y" #'counsel-yank-pop
      :desc "Send to Repl"         "r" #'+jg-repl-send-register-to-repl
      :desc "Clear All"            "K" #'+jg-registers-clear-all
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
      :desc "Search Clear" "c"                 #'evil-ex-nohighlight
      :desc "Dictionary"                   "t" #'+lookup/dictionary-definition
      :desc "Jump list"                    "j" #'evil-show-jumps
      :desc "Jump to bookmark"             "b" #'bookmark-jump
      :desc "Jump to link"                 "L" #'ffap-menu
      :desc "Jump to mark"                 "m" #'evil-show-marks
      :desc "Jump to symbol"               "i" #'imenu
      :desc "Jump to visible link"         "l" #'link-hint-open-link
      :desc "Google"                       "g" #'+jg-misc-browse-url
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
      :desc "Dictionary"                   "t" #'+lookup/dictionary-definition
      :desc "Thesaurus"                    "T" #'+lookup/synonyms
      )
;;-- end <leader> s --- search

;;-- <leader> t --- toggle
(map! :leader
      :prefix ("t" . "toggle")
      :desc "Global Company" "C" #'global-company-mode
      :desc "Input Language" "i" #'toggle-input-method
      :desc "SmartParens"    "s" #'smartparens-global-mode
      :desc "Read-only mode" "r" #'read-only-mode
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
      (:prefix ("v" . "Visual")
       ;; R
       ;; n N
       ;;
       :desc "Frame fullscreen"      "F" #'toggle-frame-fullscreen
       :desc "Evil goggles"          "g" #'evil-goggles-mode
       :desc "Hl-line"               "h" #'global-hl-line-mode
       :desc "Fill Column Indicator" "f" #'display-fill-column-indicator-mode
       :desc "Indent guides"         "i" #'highlight-indent-guides-mode
       :desc "Highlight Parens"      "(" #'global-highlight-parentheses-mode
       :desc "Prettify Mode"         "p" #'global-prettify-symbols-mode
       :desc "Word-wrap mode"        "W" #'+word-wrap-mode
       :desc "Whitespace"            "w" #'whitespace-mode
       :desc "Soft line wrapping"    "l" #'visual-line-mode
       :desc "Line Truncate"         "t" #'toggle-truncate-lines
       )
      (:prefix  ("n" . "Navigation")
       ;; i
       :desc "Neotree"             "t" #'neotree-toggle
       :desc "Minimap mode"        "m" #'minimap-mode
       :desc "org-tree-slide mode" "p" #'org-tree-slide-mode
       :desc "Centered Cursor"     "c" #'centered-cursor-mode
       :desc "Indent style"        "I" #'doom/toggle-indent-style
       :desc "Evil-visual-mark"    "v" #'evil-visual-mark-mode
       :desc "Auto-Highlight"      "h" #'auto-highlight-symbol-mode
       )
      (:when (modulep! :lang org +pomodoro)
       :desc "Pomodoro timer"             "t"   #'org-pomodoro)
      ;; highlight long lines
      ;; auto-completion
      ;; camel-case-motion
      ;; fill-column indicator
      )
;;-- end <leader> t --- toggle

;;-- <leader> W --- Workspaces
(map! :leader
      :when (modulep! :ui workspaces)
      :prefix ("W" . "Workspaces")
      ;; RET
      ;; Lowercase - workspace, Uppercase - session
      :desc "Display tab bar"              "TAB" #'+workspace/display
      :desc "Next workspace"               "]"   #'+workspace/switch-right
      :desc "Previous workspace"           "["   #'+workspace/switch-left
      :desc "Switch to last workspace"     "`"   #'+workspace/other
      :desc "Switch workspace"             "."   #'+workspace/switch-to
      :desc "Switch to last workspace"     "0"   #'+workspace/switch-to-final

      :desc "Switch workspace buffer" "b" #'persp-switch-to-buffer
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
;;-- end <leader> W --- Workspaces

;;-- <leader> w --- Windows
(map! :leader
      :prefix ("w" . "Windows")
      ;; DEL
      :desc "Delete Window" "d"      #'delete-window
      :desc "Split To Right" "/"     #'split-window-right
      :desc "Split Below" "-"        #'split-window-below
      :desc "Window up" "k"          #'evil-window-up
      :desc "Window right" "l"       #'evil-window-right
      :desc "Window left" "h"        #'evil-window-left
      :desc "Window right" "j"       #'evil-window-down
      :desc "Enlargen" "m"           #'doom/window-enlargen
      :desc "Maximize" "M"           #'doom/window-maximize-buffer
      :desc "Balance" "b"            #'balance-windows

      :desc "Shrink Horizontal" "{"  #'shrink-window-horizontally
      :desc "Shrink Vertical" "}"    #'shrink-window

      )
;;-- end <leader> w --- Windows

;;-- <leader> x -- Text
(map! :leader
      :prefix ("x" . "Text")
      )
;;-- end <leader> x -- Text

;;-- <leader> y --- snippets
(map! :leader
      :prefix ("y" . "snippets")
      ;; n
      :desc "Expand Snippet"        "y" #'yas-expand-from-trigger-key
      :desc "Edit Snippet"          "N" #'yas-visit-snippet-file
      :desc "Insert snippet"        "i" #'yas-insert-snippet
      :desc "Find global snippet"   "/" #'yas-visit-snippet-file
      :desc "Reload snippets"       "r" #'yas-reload-all
      :desc "Create Temp Template"  "c" #'aya-create
      :desc "Use Temp Template"     "e" #'aya-expand
      :desc "Find Snippet Dir"      "d" (cmd! (find-file +snippets-dir))
      )
;;-- end <leader> y --- snippets

(provide 'jg-leader-bindings-loaded)

;; Leader no prefix
;;
(dlog! "Setting up leader bindings: %s" (current-time-string))
;;-- leader
(map! :leader
      ;; "," "<" "!" "?"
      :desc "Ibuffer"               "DEL" #'ibuffer
      :desc "Jump to bookmark"      "RET" #'bookmark-jump
      :desc "Record Macro"          "SPC" #'ignore
      :desc "Find file"             "."   #'find-file
      :desc "Switch to last buffer" "TAB" #'ignore
      :desc "Split Window"          "/"   #'split-window-right
      :desc "Toggle last popup"     "`"   #'ignore

      :desc "Eval expression"       "\""   #'ignore
      :desc "M-x"                   ";"   #'ignore
      :desc "Org Capture"           "X"   #'ignore

      ;; C-u is used by evil
      :desc "Universal argument"    "u"   #'universal-argument

      ;; 0 6 9 8
      :desc "Agenda"    "7" (cmd! (find-file initial-buffer-choice))
      )

(map! :leader
      (:prefix ("B" . "Bookmarks"))
      (:prefix ("c" . "code")
       :prefix ("r" . "Repl")
       )
      (:prefix ("m" . "Local Mode"))
      (:prefix ("p". "Projects/Workspaces"))
      (:prefix ("y" . "snippets"))

      (:prefix ("x" . "Text"))

      )
;;-- end leader

;;-- <leader> b --- buffer
(map! :leader
      :prefix ("b" . "buffer")
      "DEL" #'ignore
      "-"   #'ignore
      "n"   #'ignore

      :desc "Ediff Buffers"               "TAB" #'ediff-buffers
      :desc "Next buffer"                 "]"   #'next-buffer
      :desc "Previous buffer"             "["   #'previous-buffer

      :desc "Switch buffer"               "b"   #'switch-to-buffer
      :desc "Create Buffer"               "c"   #'ignore
      :desc "Kill buffer"                 "d"   #'kill-current-buffer
      :desc "Clone Indirect"              "i"   #'clone-indirect-buffer-other-window
      :desc "Kill all buffers"            "K"   #'doom/kill-all-buffers
      :desc "Local Variables"             "l"   #'ignore

      :desc "Kill other buffers"          "O"   #'doom/kill-other-buffers
      :desc "Read-only mode"              "r"   #'read-only-mode
      :desc "Revert buffer"               "R"   #'revert-buffer
      :desc "Save all buffers"            "S"   #'ignore
      :desc "Bury buffer"                 "z"   #'bury-buffer
      )
;;-- end <leader> b --- buffer

;;--- <leader> c --- code

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
 )
;;-- end <leader> g --- git

;; -- <leader> h - help

;;-- <leader> i --- insert
(map! :leader
      :prefix ("i" . "insert")
      "d" #'ignore
      ":" #'ignore
      :desc "From Kill Ring"                "y"   #'ignore
      :desc "From register"                 "r"   #'ignore
      :desc "From Minibuffer history"       "m"   #'ignore
      :desc "Unicode"                       "u"   #'ignore
      :desc "Snippet"                       "s"   #'ignore
      )
;;-- end <leader> i --- insert

;; -- <leader> j --- journal

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

;; -- <leader> n --- notes

;;-- <leader> o --- open
(map! :leader
      :prefix ("o" . "open")
      :desc "Command History"              "DEL"  #'ignore
      :desc "Minibuffer history"           "0"    #'ignore

      :desc "Default browser"              "b"    #'browse-url-of-file
      :desc "Calc"                         "C"    #'calc-dispatch
      :desc "Dired"                        "d"    #'dired-jump
      ;; :desc "Compose Email"                "e" #'mu4e~compose-mail

      :desc "Repo Homepage"                "g"    #'+vc/browse-at-remote-homepage
      :desc "Github Homepage"              "G"    (cmd! (browse-url user-url))

      :desc "Rmail"                        "m"    #'rmail
      :desc "External Mail"                "M"    #'mu4e
      :desc "Browse notes"                 "n"    #'+default/browse-notes
      :desc "Notes"                        "N"    #'+default/find-in-notes

      :desc "Todo list"                    "t"    #'org-todo-list

      ;; (:prefix ("a" . "org agenda"))

      (:prefix ("s" . "Scratch"))
      )
;;-- end <leader> o --- open

;;-- <leader> q --- quit/restart/session
(map! :leader
      :prefix ("q" . "quit/restart")
      :desc "Quit Emacs"                   "q" #'save-buffers-kill-emacs
      :desc "Kill Emacs"                   "K" #'kill-emacs
      :desc "Pause Emacs"                  "p" #'suspend-emacs
      :desc "Kill Server"                  "s" (cmd! (server-force-stop))
      :desc "Restart emacs server"         "S" #'+default/restart-server

       (:prefix ("f" . "Frames")
        :desc "Clear current frame"          "F" #'doom/kill-all-buffers
        :desc "Delete frame"                 "f" #'delete-frame
        )
       (:prefix ("w" . "Sessions")
        :desc "Save current session"         "s" #'ignore
        :desc "Load last session"            "l" #'ignore

        :desc "Load session from file"       "L" #'ignore
        :desc "Save session to file"         "S" #'ignore
        )
      )
;;-- end <leader> q --- quit/restart/session

;; -- <leader> r -- REGISTERS (see text manip)

;; -- <leader> s --- jump/search

;; -- <leader> t --- toggle

;; -- <leader> w --- Windows

;;--- <leader> x -- Text

(provide 'jg-bindings-core)

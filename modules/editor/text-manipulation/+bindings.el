;;; util/text/+bindings.el -*- lexical-binding: t; -*-

(dlog! "Setting up text binding: %s" (current-time-string))

(local-load! "+bind-states")
(local-load! "+bind-ops")
(local-load! "+bind-motion")
(local-load! "+bind-changes")

(evil-make-intercept-map messages-buffer-mode-map)

(after! jg-global-bindings
  (keymap-global-set "C-c [" #'+jg-text-insert-lparen)
  (keymap-global-set "C-c ]" #'+jg-text-insert-rparen)
  )


(map! :leader
      :desc "Clear All"            "r K" #'+jg-text-clear-all
      :desc "Insert Random Word"   "i w" #'+jg-text-insert-random-word

      (:prefix "b"
       :desc "Yank Buffer Name" "n"   #'+jg-text-yank-buffer-name
       :desc "Clear Buffer"     "DEL" #'+jg-text-clear-buffer
       )

      (:prefix ("r" . "Registers")
       :desc "Insert Register"      "i" #'insert-register
       :desc "Save to Register"     "x" #'copy-to-register
       :desc "Windows to Register"  "w" #'window-configuration-to-register
       :desc "Jump to Register"     "j" #'jump-to-register
       :desc "List Registers"       "l" #'list-registers
       )

      (:prefix ("i" . "insert")
       :desc "From Kill Ring"                "y"   #'yank-pop
       :desc "From register"                 "r"   #'insert-register
       :desc "Unicode"                       "u"   #'insert-char
       )
)

(map! :map license-mode-map
      :localleader
      :desc "License Reference" "1" (cmd! (browse-url "https://choosealicense.com/licenses/"))
      )

(map! :map jg-dired-mode-map
      :after jg-dired-bindings
      (:prefix "c"
       :prefix ("f d" . "Pandoc")
       :desc "Make Style File" "s" #'+jg-text-pandoc-gen-style
       :desc "Compile"         "c" #'+jg-text-pandoc-compile
       )
      )

(map! :map evil-insert-state-map
      "£" (cmd! (insert "#"))
      "#" (cmd! (insert "£"))
      )


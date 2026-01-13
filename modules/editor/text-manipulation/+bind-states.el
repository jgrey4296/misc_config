;;; +bind-states.el -*- lexical-binding: t; no-byte-compile: t; -*-


(map! :map jge-normal-state-map
      :desc "SPC? Insert" "I SPC"            #'evil-mapspace-state
      :desc "Chars"       "I c"              #'evil-spechar-state
      :desc "Select Whitespace" "v SPC"      #'+jg-text-visual-select-whitespace
      )

(map! :map jgb-vision--root-map
      :prefix ("i" . "Invisible")
      :desc "Add"                       "a"   #'+jg-text-make-invisible
      :desc "Delete"                    "d"   #'+jg-text-delete-invisible
      :desc "Make Comments Invisible"   "c"   #'+jg-text-manipulation-make-comments-invisible
      :desc "Invisibilty Ivy"           "i"   #'+jg-text-manipulate-invis-spec
      :desc "Invisibilty Ivy"           "RET" #'+jg-text-manipulate-invis-spec
      :desc "Named Invisibilty"         "n"   #'+jg-text-name-invisible
      )

(map! :map jge-visual-state-map
      :desc "Grow Selection " "v g"        #'+jg-text-grow-selection-op
      :desc "Select Buffer"   "v RET"      #'+jg-text-whole-buffer-textobj
      :desc "contract"        "v SPC"      #'+jg-text-visual-contract
      :desc "Copy to Register" "v r"       #'copy-to-register
      :desc "Clone selection" "|"          #'+jg-text-yank-selection-to-new-buffer
      )

(map! :map jge-operator-state-map
      :desc "Select Line"   "g"   #'+jg-text-line-textobj
      :desc "Select Buffer" "RET" #'+jg-text-whole-buffer-textobj
      )

(map! :map jge-motion-state-map
      :desc "Goto Column"       "\\" #'+jg-text-column-motion
      )

;;; +bind-states.el ends here

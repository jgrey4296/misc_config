;;; util/base_bindings/+bindings.el -*- lexical-binding: t; -*-

;;; Main
(map! :leader

      "SPC" #'evil-avy-goto-line
      ;; Misc
      :desc "Evaluate line/region"  "e"   #'+eval/line-or-region
      :desc "Eval expression"       ";"   #'pp-eval-expression
      :desc "M-x"                   ":"   #'execute-extended-command
      :desc "Org Capture"           "X"   #'org-capture
      :desc "Pop Shell"             "'"   #'shell

      ;; C-u is used by evil
      :desc "Universal argument"    "u"   #'universal-argument
      :desc "help"                  "h"    help-map
      :desc "Split Window"          "/"    #'split-window-right
      ;; TODO pop shell

      )

;;; Toggles
(map! :leader
      :prefix ("t" . "Toggle")
      :desc "Whitespace" "w"#'whitespace-mode
      ;; centre point/line
      ;; highlight long lines
      ;; auto-completion
      ;; camel-case-motion
      ;;
      ;; fill-column indicator
      ;; indent-guide
      ;; truncate lines
      ;; line numbers
      )

;;; Windows
(map! :leader
      :prefix ("w" . "Windows")
       :desc "Delete Window" "d" #'delete-window
       :desc "Split To Right" "/" #'split-window-right
       :desc "Split Below" "-" #'split-window-below
       "k" #'evil-window-up
       "l" #'evil-window-right
       "h" #'evil-window-left
       "j" #'evil-window-down
       )

;;; Buffer
(map! :leader
      :prefix ("b" . "Buffer")
      :desc "Pop up scratch buffer" "x"   #'doom/open-scratch-buffer
      )

;;; Jumping
(map! :leader
      :prefix ("j" . "Jump")
      :desc "Jump to Line"                          "l"   #'evil-avy-goto-line
      :desc "Jump to definition"                    "d"  #'+lookup/definition
      :desc "Jump to references"                    "D"  #'+lookup/references
      :desc "Find implementations"                  "i"  #'+lookup/implementations
      :desc "Jump to documentation"                 "k"  #'+lookup/documentation
      :desc "Find type definition"                  "t"  #'+lookup/type-definition
      (:when (featurep! :completion ivy)
       :desc "Jump to symbol in current workspace" "j"  #'lsp-ivy-workspace-symbol
       :desc "Jump to symbol in any workspace"     "J"  #'lsp-ivy-global-workspace-symbol)
      (:when (featurep! :completion helm)
       :desc "Jump to symbol in current workspace" "j"  #'helm-lsp-workspace-symbol
       :desc "Jump to symbol in any workspace"     "J"  #'helm-lsp-global-workspace-symbol)
      )

;;; Local Mode
(map! (:prefix ("m" . "Local Mode")))

;;; Text
(map! :leader
      :prefix ("x" . "Text")
      :desc "Mark" "m" #'mark-whole-buffer
      "f" 'flush-lines
      "K" 'keep-lines
)
;; align
;; justify
;; upcase, downcase
;; mark buffer
;; mark line
;; string inflection and surround


;;; REGISTERS


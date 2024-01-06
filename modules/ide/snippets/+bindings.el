;;; +bindings.el -*- lexical-binding: t; -*-

;;-- browsing
(map! :leader
      (:prefix "y"
      :desc "Dir: Code"           "1" (cmd! (find-file jg-snippets-code-templates-dir))
      :desc "Dir: File"           "2" (cmd! (find-file jg-snippets-file-templates-dir))
      :desc "Dir: Projects"       "3" (cmd! (find-file jg-snippets-project-templates-dir))
      :desc "Dir: Lookup-regular" "4" (cmd! (find-file librarian-regular--location))
      :desc "Dir: Git Template"   "7" (cmd! (find-file (expand-file-name "templates/git" doom-user-dir)))
      :desc "Dir: Doom Snippet"   "8" (cmd! (find-file doom-snippets-dir))
      :desc "Dir: Yas  Snippet"   "9" (cmd! (find-file yasnippet-snippets-dir))
      :desc "Debug Snippet Dirs"  "0" #'+jg-snippets-debug-dirs
      )
      (:prefix "f"
       :desc "Debug File Template" "?" #'+file-templates/debug
       )
)
;;-- end browsing

(map! :leader ;; General
      (:prefix "i"
       :desc "Breakpoint"             "b" #'+jg-snippets-insert-debug
       :desc "Random Var"             "v" #'+snippets-random-var
       :desc "Academic"               "A" #'academic-phrases
       :desc "Academic Section"       "a" #'academic-phrases-by-section
       :desc "License"                "L" #'license-templates-insert

       (:prefix ("l" . "Lorem Ipsum")
        :desc "Sentence"         "s" #'lorem-ipsum-insert-sentences
        :desc "Paragraph"        "p" #'lorem-ipsum-insert-paragraphs
        :desc "List"             "l" #'lorem-ipsum-insert-list
        )
       )

      (:prefix "y"
      :desc "New snippet"           "n" #'+jg-snippets-new-snippet
      :desc "Expand Snippet"        "TAB" #'yas-expand-from-trigger-key
      :desc "Reload All"             "r" #'+jg-snippets-reload-all
      :desc "New Abbrev"            "N"  #'add-mode-abbrev
       )
      )

;;-- snippets

(define-key yas-minor-mode-map [menu-bar] nil)
(map! :map snippet-mode-map
      :after yasnippet
      ;; "C-c C-k" #'+snippet--abort
      :n "Q"    #'+snippet--abort
      :n "RET"  #'yas-load-snippet-buffer-and-close
      :n "T"    #'yas-tryout-snippet
      :localleader
      )
;;-- end snippets

;;-- snippet management
(map! :leader
      :prefix "y"
      :desc "Temp Template"  :v "t" (cmd! (if (eq evil-state 'visual)
                                              (setq aya-current (buffer-substring evil-visual-beginning evil-visual-end))
                                            (aya-expand)))

      (:prefix ("y"  "Yasnippet")
       :desc "Edit Snippet"          "e" #'+snippets/edit
       :desc "Find global snippet"   "f" #'yas-visit-snippet-file
      )

      (:prefix ("a" . "Abbrevs")
       :desc "Save Abbrevs"          "S"  #'+jg-snippets-save-abbrevs
       :desc "List Abbrevs"          "l"  #'list-abbrevs
       :desc  "List Local Abbrevs"   "L"  (cmd! (list-abbrevs t))
       :desc "Edit Abbrevs"          "e"  #'edit-abbrevs
       )
      )

;;-- end snippet management

;;-- insert state
(map! :map jg-binding-insert-state-map
      ;; "TAB" #'+jg-snippets-complete-or-snippet
      "TAB" #'company-complete
      ;; "TAB" #'indent-for-tab-command
      )

;;-- end insert state

;;-- shell
(map! :map shell-mode-map
      :i "TAB" #'+jg-snippets-complete-or-snippet
      :n "TAB" #'completion-at-point
      )
;;-- end shell

(map! :map jg-binding-normal-state-map
      :desc "breakpoint"          "I b"   #'+jg-snippets-insert-debug
      )
